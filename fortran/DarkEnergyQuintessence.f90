    ! Equations module allowing for fairly general quintessence models
    !
    ! by Antony Lewis (http://cosmologist.info/)

    !!FIX March 2005: corrected update to next treatment of tight coupling
    !!Fix Oct 2011: a^2 factor in ayprime(EV%w_ix+1) [thanks Raphael Flauger]
    ! Oct 2013: update for latest CAMB, thanks Nelson Lima, Martina Schwind
    ! May 2020: updated for CAMB 1.x+

    ! Notes at http://antonylewis.com/notes/CAMB.pdf

    !This module is not well tested, use at your own risk!

    !Need to specify Vofphi function, and also initial_phi
    !You may also need to change other things to get it to work with different types of quintessence model

    !It works backwards, in that it assumes Omega_de is Omega_Q today, then does a binary search on the
    !initial conditions to find what is required to give that Omega_Q today after evolution.

    ! Modifications made by João Victor Rebouças, 2021, are tagged with JVR
    ! Essentially the TEarlyQuintessence class is rebuilt to be a general quintessence class

    module Quintessence
    use DarkEnergyInterface
    use results
    use constants
    use classes
    implicit none
    private

    real(dl), parameter :: Tpl= sqrt(kappa*hbar/c**5)  ! sqrt(8 pi G hbar/c^5), reduced planck time

    ! General base class. Specific implemenetations should inherit, defining Vofphi and setting up
    ! initial conditions and interpolation tables
    type, extends(TDarkEnergyModel) :: TQuintessence
        integer :: DebugLevel = 0 ! higher then zero for some debug output to console
        real(dl) :: astart = 1e-7_dl
        real(dl) :: integrate_tol = 1e-6_dl
        real(dl), dimension(:), allocatable :: sampled_a, phi_a, phidot_a
        ! Steps for log a and linear spacing, switching at max_a_log (set by Init)
        integer, private :: npoints_linear, npoints_log
        real(dl), private :: dloga, da, log_astart, max_a_log
        real(dl), private, dimension(:), allocatable :: ddphi_a, ddphidot_a
		real(dl) :: omega_tol = 0.01
        class(CAMBdata), pointer, private :: State
    contains
    procedure :: Vofphi !V(phi) potential [+ any cosmological constant]
    procedure :: ValsAta !get phi and phi' at scale factor a, e.g. by interpolation in precomputed table
    procedure :: Init => TQuintessence_Init
    procedure :: PerturbedStressEnergy => TQuintessence_PerturbedStressEnergy
    procedure :: PerturbationEvolve => TQuintessence_PerturbationEvolve
    procedure :: BackgroundDensityAndPressure => TQuintessence_BackgroundDensityAndPressure
    procedure :: EvolveBackground
    procedure :: EvolveBackgroundLog
	procedure :: GetOmegaFromInitial ! JVR uncommented this function, which is written at the end of the file
    procedure, private :: phidot_start => TQuintessence_phidot_start
    end type TQuintessence

    ! Specific implementation for early quintessence + cosmologial constant, assuming the early component
    ! energy density fraction is negligible at z=0.
    ! The specific parameterization of the potential implemented is the axion model of arXiv:1908.06995
    type, extends(TQuintessence) :: TEarlyQuintessence ! JVR Will rename the class later, but i will have to rename it in other .f90 files so be careful
        real(dl) :: n = 3._dl ! JVR Won't use
        real(dl) :: f =0.05 ! sqrt(8*pi*G)*f Won't use
        real(dl) :: m = 5d-54 !m in reduced Planck mass units won't use
        real(dl) :: theta_i = 3.1_dl !initial value of phi/f won't use
        real(dl) :: frac_lambda0 = 0._dl !fraction of dark energy density that is cosmological constant today - could use!
        logical :: use_zc = .false. !adjust m to fit zc won't use
		
        real(dl) :: zc, fde_zc !readshift for peak f_de and f_de at that redshift won't use
        integer :: npoints = 5000 !baseline number of log a steps; will be increased if needed when there are oscillations won't use
        integer :: min_steps_per_osc = 10 !won't use
        real(dl), dimension(:), allocatable :: fde, ddfde !won't use


		! JVR - my variables for quintessence
		logical :: output_background_phi = .false. ! If the code should output a file with the scalar field evolution, phi(a). This is determined by the inifile.
		character(len=50) :: output_background_phi_filename ! The name of the file mentioned above, also determined in the inifile
		logical :: search_for_initialphi = .false. ! If the code should output a file with Omega_de x initial_phi and stop. Good for debugging/testing potentials and the binary search
		integer :: potential_type = 1 ! This sets which potential we're using. Check the function Vofphi
		real(dl) :: potentialparams = 3d-61 ! Potential parameters
        ! JVR - finished declaring my variables
    contains
    procedure :: Vofphi => TEarlyQuintessence_VofPhi
    procedure :: Init => TEarlyQuintessence_Init
    procedure :: ReadParams =>  TEarlyQuintessence_ReadParams
    procedure, nopass :: PythonClass => TEarlyQuintessence_PythonClass
    procedure, nopass :: SelfPointer => TEarlyQuintessence_SelfPointer
    procedure, private :: check_error ! Could use

    end type TEarlyQuintessence

    procedure(TClassDverk) :: dverk

    public TQuintessence, TEarlyQuintessence
    contains

    function VofPhi(this, phi, deriv) ! Can erase this function
    !Get the quintessence potential as function of phi
    !The input variable phi is sqrt(8*Pi*G)*psi, where psi is the field
    !Returns (8*Pi*G)^(1-deriv/2)*d^{deriv}V(psi)/d^{deriv}psi evaluated at psi
    !return result is in 1/Mpc^2 units [so times (Mpc/c)^2 to get units in 1/Mpc^2]
    class(TQuintessence) :: this
    real(dl) phi,Vofphi
    integer deriv

    call MpiStop('Quintessence classes must override to provide VofPhi')
    VofPhi = 0
    !if (deriv==0) then
    !    Vofphi= norm*this%m*exp(-this%sigma_model*phi) 
    !else if (deriv ==1) then
    !    Vofphi=-norm*this%m*sigma_model*exp(-this%sigma_model*phi)
    !else if (deriv ==2) then
    !    Vofphi=norm*this%m*sigma_model**2*exp(-this%sigma_model*phi)
    !else
    !    stop 'Invalid deriv in Vofphi'
    !end if
    !VofPhi = VOfPhi* MPC_in_sec**2 /Tpl**2  !convert to units of 1/Mpc^2


    end function VofPhi


    subroutine TQuintessence_Init(this, State)
    class(TQuintessence), intent(inout) :: this
    class(TCAMBdata), intent(in), target :: State

    ! JVR - This function does three things:
    ! 1 - Takes State (which is a class that carries information for all of the other variables) and passes it to the quintessence class so I can use
    ! quantities related to other variables (e.g. this%state%grho_no_de calculates the energy of all of the other components);
    ! 2 - Sets the is_cosmological_constant flag to false;
    ! 3 - Sets the number of perturbation equations to 2 (because it's a second order ODE so we split into two first-order ODEs)
    ! No modifications were made to this function.

    !Make interpolation table, etc,
    !At this point massive neutrinos have been initialized
    !so grho_no_de can be used to get density and pressure of other components at scale factor a

    select type(State)
    class is (CAMBdata)
        this%State => State
    end select

    this%is_cosmological_constant = .false.
    this%num_perturb_equations = 2

    this%log_astart = log(this%astart)

    end subroutine  TQuintessence_Init

    subroutine TQuintessence_BackgroundDensityAndPressure(this, grhov, a, grhov_t, w)
    !Get grhov_t = 8*pi*rho_de*a**2 and (optionally) equation of state at scale factor a
    class(TQuintessence), intent(inout) :: this
    real(dl), intent(in) :: grhov, a
    real(dl), intent(out) :: grhov_t
    real(dl), optional, intent(out) :: w
    real(dl) V, a2, grhov_lambda, phi, phidot

    if (this%is_cosmological_constant) then
        grhov_t = grhov * a * a
        if (present(w)) w = -1_dl
    elseif (a >= this%astart) then
        a2 = a**2
        call this%ValsAta(a,phi,phidot)
        V = this%Vofphi(phi,0)
        grhov_t = phidot**2/2 + a2*V
        if (present(w)) then
            w = (phidot**2/2 - a2*V)/grhov_t
        end if
    else
        grhov_t=0
        if (present(w)) w = -1
    end if

    end subroutine TQuintessence_BackgroundDensityAndPressure

    subroutine EvolveBackgroundLog(this,num,loga,y,yprime)
    ! Evolve the background equation in terms of loga.
    ! Variables are phi=y(1), a^2 phi' = y(2)
    ! Assume otherwise standard background components
    class(TQuintessence) :: this
    integer num
    real(dl) y(num),yprime(num)
    real(dl) loga, a

    a = exp(loga)
    call this%EvolveBackground(num, a, y, yprime)
    yprime = yprime*a

    end subroutine EvolveBackgroundLog

    subroutine EvolveBackground(this,num,a,y,yprime)
    ! Evolve the background equation in terms of a.
    ! Variables are phi=y(1), a^2 phi' = y(2)
    ! Assume otherwise standard background components
    class(TQuintessence) :: this
    integer num
    real(dl) y(num),yprime(num)
    real(dl) a, a2, tot
    real(dl) phi, grhode, phidot, adot			

    a2=a**2	! JVR - Remember that in CAMB notation dots denote derivatives with respect to conformal time
    phi = y(1)
    phidot = y(2)/a2

    grhode=a2*(0.5d0*phidot**2 + a2*this%Vofphi(phi,0)) ! JVR - This is 8*pi*G*a^4*rho. Normally, this would be 8*pi*G*a^4(psi'^2/2a^2 + V). But putting the 8*pi*G factor inside, then:
														! grhode = a^2*(phi'^2/2a^2 + 8*pi*G*V), where psi is the field in Mpc^-1 units and phi is the field in natural units
														! Vofphi(phi,0) already returns 8*pi*G*V.
														! Finally, remember that the Friedmann equation is a' = a^2 * sqrt(8*pi*G*rho/3), which is written down below
    tot = this%state%grho_no_de(a) + grhode

    adot=sqrt(tot/3.0d0)
    yprime(1)=phidot/adot ! d phi /d a
    yprime(2)= -a2**2*this%Vofphi(phi,1)/adot		! JVR - Is this right?

    end subroutine EvolveBackground


    real(dl) function TQuintessence_phidot_start(this,phi) ! JVR - I won't change this but it may be interesting to leave this here.
    class(TQuintessence) :: this
    real(dl) :: phi

    TQuintessence_phidot_start = 0

    end function TQuintessence_phidot_start

    subroutine ValsAta(this,a,aphi,aphidot)
    ! JVR - This function can be used to get phi(a) and phidot(a) by interpolation. When the Init function is called it evolves the background field
    ! so we get a grid of points phi(a_i). This function interpolates over the numerically integrated points.
    class(TQuintessence) :: this
    !Do interpolation for background phi and phidot at a (precomputed in Init)
    real(dl) a, aphi, aphidot
    real(dl) a0,b0,ho2o6,delta,da
    integer ix

    if (a >= 0.9999999d0) then
        aphi= this%phi_a(this%npoints_linear+this%npoints_log)
        aphidot= this%phidot_a(this%npoints_linear+this%npoints_log)
        return
    elseif (a < this%astart) then
        aphi = this%phi_a(1)
        aphidot = 0
        return
    elseif (a > this%max_a_log) then
        delta= a-this%max_a_log
        ix = this%npoints_log + int(delta/this%da)
    else
        delta= log(a)-this%log_astart
        ix = int(delta/this%dloga)+1
    end if
    da = this%sampled_a(ix+1) - this%sampled_a(ix)
    a0 = (this%sampled_a(ix+1) - a)/da
    b0 = 1 - a0
    ho2o6 = da**2/6._dl
    aphi=b0*this%phi_a(ix+1) + a0*(this%phi_a(ix)-b0*((a0+1)*this%ddphi_a(ix)+(2-a0)*this%ddphi_a(ix+1))*ho2o6)
    aphidot=b0*this%phidot_a(ix+1) + a0*(this%phidot_a(ix)-b0*((a0+1)*this%ddphidot_a(ix)+(2-a0)*this%ddphidot_a(ix+1))*ho2o6)

    end subroutine ValsAta

    subroutine TQuintessence_PerturbedStressEnergy(this, dgrhoe, dgqe, &
        a, dgq, dgrho, grho, grhov_t, w, gpres_noDE, etak, adotoa, k, kf1, ay, ayprime, w_ix)
    ! Get density perturbation and heat flux
    class(TQuintessence), intent(inout) :: this
    real(dl), intent(out) :: dgrhoe, dgqe
    real(dl), intent(in) ::  a, dgq, dgrho, grho, grhov_t, w, gpres_noDE, etak, adotoa, k, kf1
    real(dl), intent(in) :: ay(*)
    real(dl), intent(inout) :: ayprime(*)
    integer, intent(in) :: w_ix
    real(dl) phi, phidot, clxq, vq

    call this%ValsAta(a,phi,phidot)
    clxq=ay(w_ix)
    vq=ay(w_ix+1)
    dgrhoe= phidot*vq +clxq*a**2*this%Vofphi(phi,1)
    dgqe= k*phidot*clxq

    end subroutine TQuintessence_PerturbedStressEnergy


    subroutine TQuintessence_PerturbationEvolve(this, ayprime, w, w_ix, &
        a, adotoa, k, z, y)
    !Get conformal time derivatives of the density perturbation and velocity
    class(TQuintessence), intent(in) :: this
    real(dl), intent(inout) :: ayprime(:)
    real(dl), intent(in) :: a, adotoa, w, k, z, y(:)
    integer, intent(in) :: w_ix
    real(dl) clxq, vq, phi, phidot

    call this%ValsAta(a,phi,phidot) ! wasting time calling this again..
    clxq=y(w_ix) ! JVR - clxq = \delta \phi is the field perturbation
    vq=y(w_ix+1)
    ayprime(w_ix)= vq
    ayprime(w_ix+1) = - 2*adotoa*vq - k*z*phidot - k**2*clxq - a**2*clxq*this%Vofphi(phi,2)
    ! JVR - This is the quintessence perturbation equation in synchronous gauge (check my notes on scalar fields!)
    end subroutine TQuintessence_PerturbationEvolve

    ! Early Quintessence example, axion potential from e.g. arXiv: 1908.06995

    function TEarlyQuintessence_VofPhi(this, phi, deriv) result(VofPhi)
    !The input variable phi is sqrt(8*Pi*G)*psi
    !Returns (8*Pi*G)^(1-deriv/2)*d^{deriv}V(psi)/d^{deriv}psi evaluated at psi
	!(the (8*pi*G)^(1-deriv/2) term is because of the chain rule, since the differentiation we are performing is in the variable (phi/Mpl) = (8*pi*G)^(1/2)*phi)
    !return result is in 1/Mpc^2 units [so times (Mpc/c)^2 to get units in 1/Mpc^2]

	! JVR - Recipe to input your potential (not 100% sure about this, check units):
	! 1. Write your potential in natural units (hbar = 8*Pi*G = c = 1). The field phi has units of mass and the potential has units of mass^4
	! 2. Calculate the derivatives in natural units (d/d(phi))
	! 3. Multiply your result by units and you're done!
	! Again, this function outputs (8*pi*G)^(1-d/2)V^(d)(psi), but the module is written knowing this. Check BackgroundEvolve and PerturbationEvolve

    class(TEarlyQuintessence) :: this
    real(dl) phi,Vofphi
    integer deriv
    real(dl) theta, costheta
    real(dl), parameter :: units = MPC_in_sec**2 /Tpl**2  !convert to units of 1/Mpc^2 from natural units
	real(dl) :: m, alpha ! mass for the harmonic potential/ general power law
	integer :: n ! exponent for the generic power law V = m * phi**n
	real(dl) :: V_0, beta, u

	select case (this%potential_type)
	case(0) ! Early quintessence
		! Assume f = sqrt(kappa)*f_theory = f_theory/M_pl
		! m = m_theory/M_Pl so m is in Mpl...
		theta = phi/this%f
		if (deriv==0) then
		    Vofphi = units*this%m**2*this%f**2*(1 - cos(theta))**this%n + this%frac_lambda0*this%State%grhov !V(phi) = m²f²(1-cos(phi/f))^n
		else if (deriv ==1) then
		    Vofphi = units*this%m**2*this%f*this%n*(1 - cos(theta))**(this%n-1)*sin(theta)
		else if (deriv ==2) then
		    costheta = cos(theta)
		    Vofphi = units*this%m**2*this%n*(1 - costheta)**(this%n-1)*(this%n*(1+costheta) -1)
		end if

	case(1) ! Harmonic potential - thawing, oscillates between w = -1 and w = 1 at late times (averaging to w = 0)
		m = this%potentialparams
		if (deriv==0) then
		    Vofphi = units*m**2*phi**2/2
		else if (deriv ==1) then
		    Vofphi = units*m**2*phi
		else if (deriv ==2) then
			Vofphi = units*m**2
		end if

	case(2) ! Inverse potential, V(phi) = M^5*phi^(-1) - Has a freezing behavior
		m = this%potentialparams
		if (deriv==0) then
			Vofphi = units * m**5/phi
		else if (deriv ==1) then
			Vofphi = - units * m**5/phi**2
		else if (deriv ==2) then
			Vofphi = units * 2*m**5/phi**3
		end if

	case(3)  ! Cubic potential, V(phi) = m*phi^3/3
		m = this%potentialparams
		if (phi >= 0) then
			if (deriv==0) then
				Vofphi = m*phi**3/3
			else if (deriv ==1) then
				Vofphi = m*phi**2
			else if (deriv ==2) then
				Vofphi = 2*m*phi
			end if
		end if
		if (phi < 0) then
			if (deriv==0) then
				Vofphi = -m*phi**3/3
			else if (deriv ==1) then
				Vofphi = -m*phi**2
			else if (deriv ==2) then
				Vofphi = -2*m*phi
			end if
		end if



	case(4) ! Inverse square potential, V(phi) = M^5*phi^(-2) - Has a freezing behavior
		m = this%potentialparams
		if (deriv==0) then
			Vofphi = m/phi**2
		else if (deriv ==1) then
			Vofphi = -2*m/phi**3
		else if (deriv ==2) then
			Vofphi = 6*m/phi**4
		end if

	case(5)  ! General power law potential, V(phi) = m * phi**n
		m = this%potentialparams
		n = int(this%potentialparams)
		if (phi >= 0) then
			if (deriv==0) then
				Vofphi = m*phi**n
			else if (deriv ==1) then
				Vofphi = n*m*phi**(n-1)
			else if (deriv ==2) then
				Vofphi = n*(n-1)*m*phi**(n-2)
			end if
		end if
		if (modulo(n,2) == 0) then
			if (phi < 0) then
				if (deriv==0) then
					Vofphi = m*phi**n
				else if (deriv ==1) then
					Vofphi = n*m*phi**(n-1)
				else if (deriv ==2) then
					Vofphi = n*(n-1)*m*phi**(n-2)
				end if
			end if
		else
			if (phi < 0) then
				if (deriv==0) then
					Vofphi = -m*phi**n
				else if (deriv ==1) then
					Vofphi = -n*m*phi**(n-1)
				else if (deriv ==2) then
					Vofphi = -n*(n-1)*m*phi**(n-2)
				end if
			end if
		end if

	case(6) ! Model 1 from arxiv:1810.08586, a hyperbolic cosine well V(phi) = V_0 * cosh(beta * (phi/Mpl)**u)
		V_0 = 83.d-121
		beta = 6
		u = 1
		if (deriv==0) then
			Vofphi = units * V_0 * cosh(beta*phi**u)
		else if (deriv ==1) then
			Vofphi = units * V_0 * sinh(beta * phi**u) * beta * u * phi**(u-1)
		else if (deriv ==2) then
			Vofphi = units * V_0 * beta * u * (cosh(beta * phi**u) * beta * u * phi**(2*(u-1)) + sinh(beta * phi**u) * (u-1) * phi**(u-2))
		end if

	end select

    end function TEarlyQuintessence_VofPhi


    subroutine TEarlyQuintessence_Init(this, State)
    use Powell
    class(TEarlyQuintessence), intent(inout) :: this
    class(TCAMBdata), intent(in), target :: State
    real(dl) aend, afrom
    integer, parameter ::  NumEqs=2
    real(dl) c(24),w(NumEqs,9), y(NumEqs)
    integer ind, i, ix
    real(dl), parameter :: splZero = 0._dl
    real(dl) lastsign, da_osc, last_a, a_c ! Won't use
    real(dl) initial_phi, initial_phidot, a2
    real(dl), dimension(:), allocatable :: sampled_a, phi_a, phidot_a, fde
    integer npoints, tot_points, max_ix
    logical has_peak ! Won't use
    real(dl) fzero, xzero ! I think I won't use
    integer iflag, iter
    Type(TTimer) :: Timer
    Type(TNEWUOA) :: Minimize
    real(dl) log_params(2), param_min(2), param_max(2)

	real(dl) :: astart, atol, deltaphi, initial_phi2, om, om1, om2, phi_small, phi_large, phi, phistep, initialexp ! variables to find good initial conditions
	real(dl) :: om_small, om_large
	logical :: OK
	real(dl) :: w_phi ! JVR - equation of state
	real(dl) :: fmatter, frad ! JVR - matter and radiation fractions. Since the background is evolved here I can output the energy fractions

    !Make interpolation table, etc,
    !At this point massive neutrinos have been initialized
    !so grho_no_de can be used to get density and pressure of other components at scale factor a

    call this%TQuintessence%Init(State)

    this%dloga = (-this%log_astart)/(this%npoints-1)

    !use log spacing in a up to max_a_log, then linear. Switch where step matches
    this%max_a_log = 1.d0/this%npoints/(exp(this%dloga)-1)
    npoints = (log(this%max_a_log)-this%log_astart)/this%dloga + 1

    if (allocated(this%phi_a)) then
        deallocate(this%phi_a,this%phidot_a)
        deallocate(this%ddphi_a,this%ddphidot_a, this%sampled_a)
    end if
    allocate(phi_a(npoints),phidot_a(npoints), sampled_a(npoints), fde(npoints))

    astart=1d-9
    atol=1d-8
	if (this%search_for_initialphi .eqv. .true.) then
        ! JVR - This code bit outputs omega_phi in terms of initial_phi and stops the code. This is done in order to calibrate the binary search initial window and to
        ! check if there are ambiguities in the initial conditions
        open(unit=13, file='initialphisearch2.txt', form='formatted',status='replace')
        
        write(13, *) "initial_phi	Omega_de"
        write(13, '(2e15.6)') initial_phi, this%GetOmegaFromInitial(astart, initial_phi, 0._dl, atol)
        phistep = 1.d-6
        initialexp = -100._dl
        ! print *, MPC_in_sec**2 /Tpl**2
        do i= 1,1000
            initial_phi = 1.d-20 + i*1.d-21
            write(13, '(2e15.6)') initial_phi, this%GetOmegaFromInitial(astart, initial_phi, 0._dl, atol)
        end do
        close(13)
        stop "Forced stop - outputted omega_de x initial_phi. To undo this, set search_for_initialphi to .false."
    end if


    ! JVR - Binary search algorithm. This is an important step of the code. The current energy density of the field (and therefore Omega_de)
    ! are determined by the initial conditions phi(astart), phidot(astart). Since the Hubble friction 3Hphidot can be arbitrarily large, any initial
    ! field velocity is dissipated and the initial kinetic energy is negligible (check my notes on initial conditions). Thus, the current quintessence
    ! energy density is determined by phi(astart). The dark energy density parameter is set by the condition Omega_de = 1 - Omega_k - Omega_matter - Omega_radiation - ...
    ! so in this scheme the initial condition for the background field is set by the cosmological parameter closure relation. To find the correct value for initial_phi,
    ! we do a binary search (also known as the bissection method for finding roots of equations).

    ! Set initial conditions to give correct Omega_de now
    ! JVR - The initial window is important because it needs to encompass the correct value (assuming there is only one, but it may not be true)
    initial_phi  = 0.01_dl  ! JVR - Remember that this is in Mpl
    initial_phi2 = 100._dl
    
    initial_phidot =  astart*this%phidot_start(initial_phi)
    om1= this%GetOmegaFromInitial(astart,initial_phi,initial_phidot, atol)

    print*, 'Target omega_de: ', this%state%Omega_de, 'First trial:', om1, 'Second trial:', om2
    if (abs(om1 - this%state%Omega_de) > this%omega_tol) then 
        ! JVR - If our initial guess is not good, enter the algorithm
        OK=.false.
        initial_phidot = astart*this%phidot_start(initial_phi2)
        om2= this%GetOmegaFromInitial(astart,initial_phi2,initial_phidot, atol)

        if ((om1 < this%state%Omega_de .and. om2 < this%state%Omega_de) .or. &
		 (om1 > this%state%Omega_de .and. om2 > this%state%Omega_de)) then
			! JVR - The bissection algorithm only works if one of the window boundaries is below the required value and
			! the other is above the required value. If this is not the case, adjust the window.
            write (*,*) 'The initial phi window must bracket required value: ', this%state%Omega_de
            write (*,*) 'om1, om2 = ', real(om1), real(om2)
            stop
        end if
		
		if (om1 < this%state%Omega_de) then
			phi_small = initial_phi
			om_small = om1
			phi_large = initial_phi2
			om_large = om2
		else
			phi_small = initial_phi2
			om_small = om2
			phi_large = initial_phi
			om_large = om1
		end if

        do iter=1,100 ! JVR - Dividing the window in half 100 times. The code should leave the loop way before the last iteration
            deltaphi = phi_large - phi_small ! Window size
            phi = phi_small + deltaphi/2 ! Middle value
            initial_phidot =  astart*this%phidot_start(phi)
            om = this%GetOmegaFromInitial(astart,phi,initial_phidot,atol)
            if (om < this%state%Omega_de) then
                om_small=om
                phi_small=phi
            else
                om_large=om
                phi_large=phi
            end if
            if (abs(om_large-om_small) < 1d-3) then ! JVR - The omega tolerance is 10^(-3)
                OK=.true.
                initial_phi = (phi_small + phi_large)/2
                if (FeedbackLevel > 0) write(*,*) 'phi_initial = ',initial_phi, 'omega = ', this%GetOmegaFromInitial(astart, initial_phi, 0._dl, atol)
				!stop
                exit
            end if
    
        end do
        if (.not. OK) stop 'Search for good intial conditions did not converge' ! This should not happen
    
    end if ! Leaving binary search algorithm
	!initial_phi = 1.d-3
    !initial_phi = 1d-5 ! The code came with an initial value of 0.15Mpl

    y(1)=initial_phi
    initial_phidot =  this%astart*this%phidot_start(initial_phi)
    y(2)= initial_phidot*this%astart**2

    phi_a(1)=y(1)
    phidot_a(1)=y(2)/this%astart**2
    sampled_a(1)=this%astart
    da_osc = 1 ! Won't use
    last_a = this%astart
    max_ix =0

    ind=1
    afrom=this%log_astart

	! Modifying to output background phi(a)
	if (this%output_background_phi .eqv. .true.) then
		open(unit=50, file=this%output_background_phi_filename, form='formatted', status='replace')
		write(50, *) "a		phi		phidot		fde		w	1+z	fmatter	frad"
	end if
    do i=1, npoints-1
        aend = this%log_astart + this%dloga*i
        ix = i+1
        sampled_a(ix)=exp(aend)
        a2 = sampled_a(ix)**2
        call dverk(this,NumEqs,EvolveBackgroundLog,afrom,y,aend,this%integrate_tol,ind,c,NumEqs,w)
        if (.not. this%check_error(exp(afrom), exp(aend))) return
        call EvolveBackgroundLog(this,NumEqs,aend,y,w(:,1))
        phi_a(ix)=y(1)
        phidot_a(ix)=y(2)/a2

        ! JVR - fde is the dark energy fraction
        fde(ix) = 1/((this%state%grho_no_de(sampled_a(ix)) +  this%frac_lambda0*this%State%grhov*a2**2) &
            /(a2*(0.5d0* phidot_a(ix)**2 + a2*this%Vofphi(phi_a(ix),0))) + 1)

		fmatter = this%state%grho_matter(sampled_a(ix)) / (this%state%grho_no_de(sampled_a(ix)) + (a2*(0.5d0* phidot_a(ix)**2 + a2*this%Vofphi(phi_a(ix),0))))

		frad = this%state%grho_radiation(sampled_a(ix))/(this%state%grho_no_de(sampled_a(ix)) + (a2*(0.5d0* phidot_a(ix)**2 + a2*this%Vofphi(phi_a(ix),0))))

		! w_phi is the quintessence Eos parameter
		w_phi = (phidot_a(ix)**2/2 - a2*this%Vofphi(phi_a(ix),0))/(phidot_a(ix)**2/2 + a2*this%Vofphi(phi_a(ix),0))

		if (this%output_background_phi .eqv. .true.) then ! Output background evolution
			write(50, '(8e16.6)') sampled_a(ix), phi_a(ix), phidot_a(ix), fde(ix), w_phi, 1._dl/sampled_a(ix), fmatter, frad
		end if
		
		! Also won't need this if
        if (sampled_a(ix)*(exp(this%dloga)-1)*this%min_steps_per_osc > da_osc) then
            !Step size getting too big to sample oscillations well
            exit
        end if

    end do

    ! Do remaining steps with linear spacing in a, trying to be small enough
    this%npoints_log = ix
    this%max_a_log = sampled_a(ix)
    this%da = min(this%max_a_log *(exp(this%dloga)-1), &
        da_osc/this%min_steps_per_osc, (1- this%max_a_log)/(this%npoints-this%npoints_log))
    this%npoints_linear = int((1- this%max_a_log)/ this%da)+1
    this%da = (1- this%max_a_log)/this%npoints_linear

    tot_points = this%npoints_log+this%npoints_linear
    allocate(this%phi_a(tot_points),this%phidot_a(tot_points))
    allocate(this%ddphi_a(tot_points),this%ddphidot_a(tot_points))
    allocate(this%sampled_a(tot_points), this%fde(tot_points), this%ddfde(tot_points))
    this%sampled_a(1:ix) = sampled_a(1:ix)
    this%phi_a(1:ix) = phi_a(1:ix)
    this%phidot_a(1:ix) = phidot_a(1:ix)
    this%sampled_a(1:ix) = sampled_a(1:ix)
    this%fde(1:ix) = fde(1:ix)

    ind=1
    afrom = this%max_a_log
    do i=1, this%npoints_linear
        ix = this%npoints_log + i
        aend = this%max_a_log + this%da*i
        a2 =aend**2
        this%sampled_a(ix)=aend
        call dverk(this,NumEqs,EvolveBackground,afrom,y,aend,this%integrate_tol,ind,c,NumEqs,w)
        if (.not. this%check_error(afrom, aend)) return
        call EvolveBackground(this,NumEqs,aend,y,w(:,1))
        this%phi_a(ix)=y(1)
        this%phidot_a(ix)=y(2)/a2

        this%fde(ix) = 1/((this%state%grho_no_de(aend) +  this%frac_lambda0*this%State%grhov*a2**2) &
            /(a2*(0.5d0* this%phidot_a(ix)**2 + a2*this%Vofphi(y(1),0))) + 1)

		w_phi = (0.5d0 * this%phidot_a(ix)**2 - a2*this%Vofphi(this%phi_a(ix),0))/(0.5d0 * this%phidot_a(ix)**2 + a2*this%Vofphi(this%phi_a(ix),0))

		fmatter = this%state%grho_matter(this%sampled_a(ix)) / (this%state%grho_no_de(this%sampled_a(ix)) + (a2*(0.5d0* this%phidot_a(ix)**2 + a2*this%Vofphi(this%phi_a(ix),0))))

		frad = this%state%grho_radiation(this%sampled_a(ix))/(this%state%grho_no_de(this%sampled_a(ix)) + (a2*(0.5d0* this%phidot_a(ix)**2 + a2*this%Vofphi(this%phi_a(ix),0))))

		if (this%output_background_phi .eqv. .true.) then ! Output background evolution
			write(50, '(8e16.6)') this%sampled_a(ix), this%phi_a(ix), this%phidot_a(ix), this%fde(ix), w_phi, 1._dl/this%sampled_a(ix), fmatter, frad
		end if

    end do
    if (this%output_background_phi .eqv. .true.) then
		close(50)
	end if
	
    call spline(this%sampled_a,this%phi_a,tot_points,splZero,splZero,this%ddphi_a)
    call spline(this%sampled_a,this%phidot_a,tot_points,splZero,splZero,this%ddphidot_a)
    call spline(this%sampled_a,this%fde,tot_points,splZero,splZero,this%ddfde)

    end subroutine TEarlyQuintessence_Init

    logical function check_error(this, afrom, aend)
    class(TEarlyQuintessence) :: this
    real(dl) afrom, aend

    if (global_error_flag/=0) then
        write(*,*) 'TEarlyQuintessence error integrating', afrom, aend
        write(*,*) this%n, this%f, this%m, this%theta_i
        stop
        check_error = .false.
        return
    end if
    check_error= .true.
    end function check_error

    subroutine TEarlyQuintessence_ReadParams(this, Ini)
    ! JVR - This function reads information from the inifile.
    use IniObjects
    class(TEarlyQuintessence) :: this
    class(TIniFile), intent(in) :: Ini

    call this%TDarkEnergyModel%ReadParams(Ini)

	! Should CAMB output phi(a)? In which file?
	this%output_background_phi = Ini%Read_Logical('output_scalarfield')
	this%output_background_phi_filename = Ini%Read_String('output_scalarfield_filename')

	! Reading potential type and parameters
	this%potential_type = Ini%Read_Int('potentialtype', 0)
	this%potentialparams = Ini%Read_Double('potentialparam1', 0._dl)
	! this%potentialparams = Ini%Read_Double('potentialparam2', 0._dl)

    end subroutine TEarlyQuintessence_ReadParams


    function TEarlyQuintessence_PythonClass()
    character(LEN=:), allocatable :: TEarlyQuintessence_PythonClass

    TEarlyQuintessence_PythonClass = 'EarlyQuintessence'

    end function TEarlyQuintessence_PythonClass

    subroutine TEarlyQuintessence_SelfPointer(cptr,P)
    use iso_c_binding
    Type(c_ptr) :: cptr
    Type (TEarlyQuintessence), pointer :: PType
    class (TPythonInterfacedClass), pointer :: P

    call c_f_pointer(cptr, PType)
    P => PType

    end subroutine TEarlyQuintessence_SelfPointer


    real(dl) function GetOmegaFromInitial(this, astart,phi,phidot,atol)
    !Get Omega_de today given particular conditions phi and phidot at a=astart
    class(TQuintessence) :: this
    real(dl), intent(IN) :: astart, phi,phidot, atol
    integer, parameter ::  NumEqs=2
    real(dl) c(24),w(NumEqs,9), y(NumEqs), ast
    integer ind, i
    
    ast=astart
    ind=1
    y(1)=phi
    y(2)=phidot*astart**2
    call dverk(this,NumEqs,EvolveBackground,ast,y,1._dl,atol,ind,c,NumEqs,w)
    call EvolveBackground(this,NumEqs,1._dl,y,w(:,1))
    
    GetOmegaFromInitial=(0.5d0*y(2)**2 + this%Vofphi(y(1),0))/this%State%grhocrit !(3*adot**2)
    
    end function GetOmegaFromInitial
	
    end module Quintessence
