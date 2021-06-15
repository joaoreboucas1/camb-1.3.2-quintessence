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
        integer :: DebugLevel = 0 !higher then zero for some debug output to console
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
	procedure :: GetOmegaFromInitial
    procedure, private :: phidot_start => TQuintessence_phidot_start
    end type TQuintessence

    ! Specific implementation for early quintessence + cosmologial constant, assuming the early component
    ! energy density fraction is negligible at z=0.
    ! The specific parameterization of the potential implemented is the axion model of arXiv:1908.06995
    type, extends(TQuintessence) :: TEarlyQuintessence ! Can rename later
        real(dl) :: n = 3._dl
        real(dl) :: f =0.05 ! sqrt(8*pi*G)*f
        real(dl) :: m = 5d-54 !m in reduced Planck mass units
        real(dl) :: theta_i = 3.1_dl !initial value of phi/f
        real(dl) :: frac_lambda0 = 0._dl !fraction of dark energy density that is cosmological constant today
        logical :: use_zc = .false. !adjust m to fit zc
		
        real(dl) :: zc, fde_zc !readshift for peak f_de and f_de at that redshift
        integer :: npoints = 5000 !baseline number of log a steps; will be increased if needed when there are oscillations
        integer :: min_steps_per_osc = 10
        real(dl), dimension(:), allocatable :: fde, ddfde


		!!!!! My variables for quintessence
		logical :: output_background_phi = .false. ! If the code should output a file with the scalar field evolution, phi(a). This is determined by the inifile.
		character(len=50) :: output_background_phi_filename ! The name of the file mentioned above, also determined in the inifile
		logical :: search_for_initialphi = .false. ! If the code should output a file with Omega_de x initial_phi. Good for debugging and testing potentials
		logical :: outputinitialphivsm = .false. ! Outputting initial_phi for different values of m
		integer :: potential_type = 1 ! 0 for the early quintessence, 1 for m²phi²/2
		real(dl) :: potentialparams = 3d-61
    contains
    procedure :: Vofphi => TEarlyQuintessence_VofPhi
    procedure :: Init => TEarlyQuintessence_Init
    procedure :: ReadParams =>  TEarlyQuintessence_ReadParams
    procedure, nopass :: PythonClass => TEarlyQuintessence_PythonClass
    procedure, nopass :: SelfPointer => TEarlyQuintessence_SelfPointer
    procedure, private :: fdeAta
    procedure, private :: fde_peak
    procedure, private :: check_error
    procedure :: calc_zc_fde

    end type TEarlyQuintessence

    procedure(TClassDverk) :: dverk

    public TQuintessence, TEarlyQuintessence
    contains

    function VofPhi(this, phi, deriv)
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

    a2=a**2	! Dots denote derivatives with respect to 
    phi = y(1)
    phidot = y(2)/a2

    grhode=a2*(0.5d0*phidot**2 + a2*this%Vofphi(phi,0)) ! This is 8*pi*G*a^4*rho. Normally, this would be 8*pi*G*a^4(psi'^2/2a^2 + V). But putting the 8*pi*G factor inside, then:
														! grhode = a^2*(phi'^2/2a^2 + 8*pi*G*V), where psi is the field in Mpc units and phi is the field in natural units
														! Vofphi(phi,0) already returns 8*pi*G*V.
														! Finally, remember that the Friedmann equation is a' = a^2 * sqrt(8*pi*G*rho/3), which is written down below
    tot = this%state%grho_no_de(a) + grhode

    adot=sqrt(tot/3.0d0)
    yprime(1)=phidot/adot ! d phi /d a
    yprime(2)= -a2**2*this%Vofphi(phi,1)/adot		! Is this right? Check!!

    end subroutine EvolveBackground


    real(dl) function TQuintessence_phidot_start(this,phi)
    class(TQuintessence) :: this
    real(dl) :: phi

    TQuintessence_phidot_start = 0

    end function TQuintessence_phidot_start

    subroutine ValsAta(this,a,aphi,aphidot)
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
    !Get density perturbation and heat flux
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

    call this%ValsAta(a,phi,phidot) !wasting time calling this again..
    clxq=y(w_ix)
    vq=y(w_ix+1)
    ayprime(w_ix)= vq
    ayprime(w_ix+1) = - 2*adotoa*vq - k*z*phidot - k**2*clxq - a**2*clxq*this%Vofphi(phi,2)

    end subroutine TQuintessence_PerturbationEvolve

    ! Early Quintessence example, axion potential from e.g. arXiv: 1908.06995

    function TEarlyQuintessence_VofPhi(this, phi, deriv) result(VofPhi)
    !The input variable phi is sqrt(8*Pi*G)*psi
    !Returns (8*Pi*G)^(1-deriv/2)*d^{deriv}V(psi)/d^{deriv}psi evaluated at psi
	!(the (8*pi*G)^(1-deriv/2) term is because of the chain rule, since the differentiation we are performing is in the variable (phi/Mpl) = (8*pi*G)^(1/2)*phi)
    !return result is in 1/Mpc^2 units [so times (Mpc/c)^2 to get units in 1/Mpc^2]

	! Recipe to input your potential:
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
    real(dl) lastsign, da_osc, last_a, a_c
    real(dl) initial_phi, initial_phidot, a2
    real(dl), dimension(:), allocatable :: sampled_a, phi_a, phidot_a, fde
    integer npoints, tot_points, max_ix
    logical has_peak
    real(dl) fzero, xzero
    integer iflag, iter
    Type(TTimer) :: Timer
    Type(TNEWUOA) :: Minimize
    real(dl) log_params(2), param_min(2), param_max(2)

	real(dl) :: astart, atol, deltaphi, initial_phi2, om, om1, om2, phi_small, phi_large, phi, phistep, initialexp ! variables to find good initial conditions
	real(dl) :: om_small, om_large
	logical :: OK
	real(dl) :: w_phi ! equation of state
	real(dl) :: fmatter, frad

    !Make interpolation table, etc,
    !At this point massive neutrinos have been initialized
    !so grho_no_de can be used to get density and pressure of other components at scale factor a

    call this%TQuintessence%Init(State)
	print *, "!", this%potentialparams, this%potential_type

    if (this%use_zc) then
        !Find underlying parameters m,f to give specified zc and fde_zc (peak early dark energy fraction)
        !Input m,f are used as starting values for search, which is done by brute force
        !(so should generalize easily, but not optimized for this specific potential)
        log_params(1) = log(this%f)
        log_params(2) = log(this%m)

        if (.false.) then
            ! Can just iterate linear optimizations when nearly orthogonal
            call Timer%Start()
            do iter = 1, 2
                call brentq(this,match_fde,log(0.01_dl),log(10._dl), 1d-3,xzero,fzero,iflag)
                if (iflag/=0) print *, 'BRENTQ FAILED f'
                this%f = exp(xzero)
                print *, 'match to m, f =', this%m, this%f, fzero
                call brentq(this,match_zc,log(1d-55),log(1d-52), 1d-3,xzero,fzero,iflag)
                if (iflag/=0) print *, 'BRENTQ FAILED m'
                this%m = exp(xzero)
                print *, 'match to m, f =', this%m, this%f, fzero
                call this%calc_zc_fde(fzero, xzero)
                print *, 'matched outputs', fzero, xzero
            end do
            call Timer%WriteTime('Timing for fitting')
        end if
        if (this%DebugLevel>0) call Timer%Start()
        !Minimize in log f, log m
        ! param_min(1) = log(0.001_dl)
        ! param_min(2) = log(1d-58)
        ! param_max(1) = log(1e5_dl)
        ! param_max(2) = log(1d-50)
        ! if (Minimize%BOBYQA(this, match_fde_zc, 2, 5, log_params,param_min, &
        !           param_max, 0.8_dl,1e-4_dl,this%DebugLevel,2000)) then

        if (Minimize%NEWUOA(this, match_fde_zc, 2, 5, log_params,&
            0.8_dl,1e-4_dl,this%DebugLevel,500)) then

            if (Minimize%Last_bestfit > 1e-3) then
                global_error_flag = error_darkenergy
                global_error_message= 'TEarlyQuintessence ERROR converging solution for fde, zc'
                write(*,*) 'last-bestfit= ', Minimize%Last_bestfit
                return
            end if
            this%f = exp(log_params(1))
            this%m = exp(log_params(2))
            if (this%DebugLevel>0) then
                call this%calc_zc_fde(fzero, xzero)
                write(*,*) 'matched outputs Bobyqa zc, fde = ', fzero, xzero
            end if
        else
            global_error_flag = error_darkenergy
            global_error_message= 'TEarlyQuintessence ERROR finding solution for fde, zc'
            return
        end if
        if (this%DebugLevel>0) call Timer%WriteTime('Timing for parameter fitting')
    end if

    this%dloga = (-this%log_astart)/(this%npoints-1)

    !use log spacing in a up to max_a_log, then linear. Switch where step matches
    this%max_a_log = 1.d0/this%npoints/(exp(this%dloga)-1)
    npoints = (log(this%max_a_log)-this%log_astart)/this%dloga + 1

    if (allocated(this%phi_a)) then
        deallocate(this%phi_a,this%phidot_a)
        deallocate(this%ddphi_a,this%ddphidot_a, this%sampled_a)
    end if
    allocate(phi_a(npoints),phidot_a(npoints), sampled_a(npoints), fde(npoints))


	! Set initial conditions to give correct Omega_de now
    initial_phi  = 0.01_dl  !  Remember that this is in Mpl
    initial_phi2 = 100._dl
    
    
    astart=1d-9
    atol=1d-8
    initial_phidot =  astart*this%phidot_start(initial_phi)
    om1= this%GetOmegaFromInitial(astart,initial_phi,initial_phidot, atol)

	if (this%search_for_initialphi .eqv. .true.) then
	! This code bit outputs omega_phi in terms of initial_phi, in order to calibrate the binary search initial window and to
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
	stop "outputted omega_de x initial_phi"
	end if

		
	! This code bit is the binary search: starting with a considerably huge window of initial_phi, we want to find
	! the value that satisfies omega_phi = 1 - Omega_m - Omega_r - Omega_k. This is done by the bissection algorithm
	
    print*, 'Target omega_de: ', this%state%Omega_de, 'first trial:', om1
    if (abs(om1 - this%state%Omega_de) > this%omega_tol) then 
        ! If our initial guess is not good, enter the algorithm
        OK=.false.
        initial_phidot = astart*this%phidot_start(initial_phi2)
        om2= this%GetOmegaFromInitial(astart,initial_phi2,initial_phidot, atol)

        if ((om1 < this%state%Omega_de .and. om2 < this%state%Omega_de) .or. &
		 (om1 > this%state%Omega_de .and. om2 > this%state%Omega_de)) then
			! The bissection algorithm only works if one of the window boundaries is below the required value and
			! the other is above the required value
            write (*,*) 'initial phi tentative values must bracket required value.  '
            write (*,*) 'om1, om2 = ', real(om1), real(om2)
            stop
        end if
		
		! I have adapted the algorithm because for some potentials, Omega_phi is a descending function of initial_phi
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

        do iter=1,1000 ! Dividing the window in half 100 times
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
            if (abs(om_large-om_small) < 1d-3) then ! When the window size gets smaller than this, we leave the algorithm
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
    da_osc = 1
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

		! I think I don't need this if
        if (i==1) then
            lastsign = y(2)
        elseif (y(2)*lastsign < 0) then
            !derivative has changed sign. Use to probe any oscillation scale:
            da_osc = min(da_osc, exp(aend) - last_a)
            last_a = exp(aend)
            lastsign= y(2)
        end if

        !Define fde as ratio of early dark energy density to total
        fde(ix) = 1/((this%state%grho_no_de(sampled_a(ix)) +  this%frac_lambda0*this%State%grhov*a2**2) &
            /(a2*(0.5d0* phidot_a(ix)**2 + a2*this%Vofphi(phi_a(ix),0))) + 1)

		fmatter = this%state%grho_matter(sampled_a(ix)) / (this%state%grho_no_de(sampled_a(ix)) + (a2*(0.5d0* phidot_a(ix)**2 + a2*this%Vofphi(phi_a(ix),0))))

		frad = this%state%grho_radiation(sampled_a(ix))/(this%state%grho_no_de(sampled_a(ix)) + (a2*(0.5d0* phidot_a(ix)**2 + a2*this%Vofphi(phi_a(ix),0))))

		! w_phi is the quintessence Eos parameter
		w_phi = (phidot_a(ix)**2/2 - a2*this%Vofphi(phi_a(ix),0))/(phidot_a(ix)**2/2 + a2*this%Vofphi(phi_a(ix),0))
        if (max_ix==0 .and. ix > 2 .and. fde(ix)< fde(ix-1)) then
            max_ix = ix-1
        end if

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

        if (max_ix==0 .and. this%fde(ix)< this%fde(ix-1)) then
            max_ix = ix-1
        end if
    end do
	close(50)
    call spline(this%sampled_a,this%phi_a,tot_points,splZero,splZero,this%ddphi_a)
    call spline(this%sampled_a,this%phidot_a,tot_points,splZero,splZero,this%ddphidot_a)
    call spline(this%sampled_a,this%fde,tot_points,splZero,splZero,this%ddfde)
    has_peak = .false.
    if (max_ix >0) then
        ix = max_ix
        has_peak = this%fde_peak(a_c, this%sampled_a(ix), this%sampled_a(ix+1), this%fde(ix), &
            this%fde(ix+1), this%ddfde(ix), this%ddfde(ix+1))
        if (.not. has_peak) then
            has_peak = this%fde_peak(a_c, this%sampled_a(ix-1), this%sampled_a(ix), &
                this%fde(ix-1), this%fde(ix), this%ddfde(ix-1), this%ddfde(ix))
        end if
    end if
    if (has_peak) then
        this%zc = 1/a_c-1
        this%fde_zc = this%fdeAta(a_c)
    else
        if (this%DebugLevel>0) write(*,*) 'TEarlyQuintessence: NO PEAK '
        this%zc = -1
    end if
    if (this%DebugLevel>0) then
        write(*,*) 'TEarlyQuintessence zc, fde used', this%zc, this%fde_zc
    end if

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

    logical function fde_peak(this, peak, xlo, xhi, Flo, Fhi, ddFlo, ddFhi)
    class(TEarlyQuintessence) :: this
    real(dl), intent(out) :: peak
    real(dl) Delta
    real(dl), intent(in) :: xlo, xhi, ddFlo, ddFhi,Flo, Fhi
    real(dl) a, b, c, fac

    !See if derivative has zero in spline interval xlo .. xhi

    Delta = xhi - xlo

    a = 0.5_dl*(ddFhi-ddFlo)/Delta
    b = (xhi*ddFlo-xlo*ddFhi)/Delta
    c = (Fhi-Flo)/Delta+ Delta/6._dl*((1-3*xhi**2/Delta**2)*ddFlo+(3*xlo**2/Delta**2-1)*ddFhi)
    fac = b**2-4*a*c
    if (fac>=0) then
        fac = sqrt(fac)
        peak = (-b + fac)/2/a
        if (peak >= xlo .and. peak <= xhi) then
            fde_peak = .true.
            return
        else
            peak = (-b - fac)/2/a
            if (peak >= xlo .and. peak <= xhi) then
                fde_peak = .true.
                return
            end if
        end if
    end if
    fde_peak = .false.

    end function fde_peak

    function match_zc(this, logm)
    class(TEarlyQuintessence), intent(inout) :: this
    real(dl), intent(in) :: logm
    real(dl) match_zc, zc, fde_zc

    this%m = exp(logm)
    call this%calc_zc_fde(zc, fde_zc)
    match_zc = zc - this%zc

    end function match_zc

    function match_fde(this, logf)
    class(TEarlyQuintessence), intent(inout) :: this
    real(dl), intent(in) :: logf
    real(dl) match_fde, zc, fde_zc

    this%f = exp(logf)
    call this%calc_zc_fde(zc, fde_zc)
    match_fde = fde_zc - this%fde_zc

    end function match_fde

    function match_fde_zc(this, x)
    class(TEarlyQuintessence) :: this
    real(dl), intent(in) :: x(:)
    real(dl) match_fde_zc, zc, fde_zc

    this%f = exp(x(1))
    this%m = exp(x(2))
    call this%calc_zc_fde(zc, fde_zc)

    match_fde_zc = (log(this%fde_zc)-log(fde_zc))**2 + (log(zc)-log(this%zc))**2
    if (this%DebugLevel>1) then
        write(*,*) 'search f, m, zc, fde_zc, chi2', this%f, this%m, zc, fde_zc, match_fde_zc
    end if

    end function match_fde_zc

    subroutine calc_zc_fde(this, z_c, fde_zc)
    class(TEarlyQuintessence), intent(inout) :: this
    real(dl), intent(out) :: z_c, fde_zc
    real(dl) aend, afrom
    integer, parameter ::  NumEqs=2
    real(dl) c(24),w(NumEqs,9), y(NumEqs)
    integer ind, i, ix
    real(dl), parameter :: splZero = 0._dl
    real(dl) a_c
    real(dl) initial_phi, initial_phidot, a2
    real(dl), dimension(:), allocatable :: sampled_a, fde, ddfde
    integer npoints, max_ix
    logical has_peak
    real(dl) a0, b0, da

    ! Get z_c and f_de(z_c) where z_c is the redshift of (first) peak of f_de (de energy fraction)
    ! Do this by forward propagating until peak, then get peak values by cubic interpolation

    initial_phi = this%theta_i*this%f
    this%log_astart = log(this%astart)
    this%dloga = (-this%log_astart)/(this%npoints-1)

    npoints = (-this%log_astart)/this%dloga + 1
    allocate(sampled_a(npoints), fde(npoints), ddfde(npoints))

    y(1)=initial_phi
    initial_phidot =  this%astart*this%phidot_start(initial_phi)
    y(2)= initial_phidot*this%astart**2
    sampled_a(1)=this%astart
    max_ix =0
    ind=1
    afrom=this%log_astart
    do i=1, npoints-1
        aend = this%log_astart + this%dloga*i
        ix = i+1
        sampled_a(ix)=exp(aend)
        a2 = sampled_a(ix)**2
        call dverk(this,NumEqs,EvolveBackgroundLog,afrom,y,aend,this%integrate_tol,ind,c,NumEqs,w)
        if (.not. this%check_error(exp(afrom), exp(aend))) return
        call EvolveBackgroundLog(this,NumEqs,aend,y,w(:,1))
        fde(ix) = 1/((this%state%grho_no_de(sampled_a(ix)) +  this%frac_lambda0*this%State%grhov*a2**2) &
            /((0.5d0*y(2)**2/a2 + a2**2*this%Vofphi(y(1),0))) + 1)
        if (max_ix==0 .and. ix > 2 .and. fde(ix)< fde(ix-1)) then
            max_ix = ix-1
        end if
        if (max_ix/=0 .and. ix > max_ix+4) exit
    end do

    call spline(sampled_a,fde,ix,splZero,splZero,ddfde)
    has_peak = .false.
    if (max_ix >0) then
        has_peak = this%fde_peak(a_c, sampled_a(max_ix), sampled_a(max_ix+1), fde(max_ix), &
            fde(max_ix+1), ddfde(max_ix), ddfde(max_ix+1))
        if (.not. has_peak) then
            has_peak = this%fde_peak(a_c, sampled_a(max_ix-1), sampled_a(max_ix), &
                fde(max_ix-1), fde(max_ix), ddfde(max_ix-1), ddfde(max_ix))
        end if
    end if
    if (has_peak) then
        z_c = 1/a_c-1
        ix = int((log(a_c)-this%log_astart)/this%dloga)+1
        da = sampled_a(ix+1) - sampled_a(ix)
        a0 = (sampled_a(ix+1) - a_c)/da
        b0 = 1 - a0
        fde_zc=b0*fde(ix+1) + a0*(fde(ix)-b0*((a0+1)*ddfde(ix)+(2-a0)*ddfde(ix+1))*da**2/6._dl)
    else
        write(*,*) 'calc_zc_fde: NO PEAK'
        z_c = -1
        fde_zc = 0
    end if

    end subroutine calc_zc_fde

    function fdeAta(this,a)
    class(TEarlyQuintessence) :: this
    real(dl), intent(in) :: a
    real(dl) fdeAta, aphi, aphidot, a2

    call this%ValsAta(a, aphi, aphidot)
    a2 = a**2
    fdeAta = 1/((this%state%grho_no_de(a) +  this%frac_lambda0*this%State%grhov*a2**2) &
        /(a2*(0.5d0* aphidot**2 + a2*this%Vofphi(aphi,0))) + 1)
    end function fdeAta

    subroutine TEarlyQuintessence_ReadParams(this, Ini)
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
