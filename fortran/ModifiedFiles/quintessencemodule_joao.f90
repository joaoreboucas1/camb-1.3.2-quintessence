! Quintessence module for CAMB
! Author: João Victor Silva Rebouças
! Log:
! 12/03/21: started to write.

module Quintessence_Joao
use DarkEnergyInterface ! Contains some relevant quantities I can use
use classes ! Contains the other classes so I can use other quantities
use constants ! I will need to convert the field unit and the potential unit.

real(dl), parameter :: Tpl= sqrt(kappa*hbar/c**5)  ! sqrt(8 pi G hbar/c^5), reduced planck time

type TQuintessence_Joao ! Maybe extend TDarkEnergyModel but not necessary. Perhaps will need an array of field values
real(dl) :: phi, phidot ! phi = field, phidot = dphi/dtau
real(dl) :: dphi, dphidot ! dphi - field perturbation in synch gauge, dphidot = d(dphi)/dtau
integer :: potential_type ! Allows me to change the potential
real(dl), dimension(3) :: potentialparams ! Parameters for the potential V(phi).  I think I will only work with at most 3 of them but this can change
integer :: 

contains
! Relevant field quantities, both background and linear perturbations, using synchronous gauge
procedure :: field_potential
procedure ::  field_kineticenergy
procedure :: field_totalenergy
procedure :: field_eos
procedure :: field_densityperturbation
procedure :: field_pressureperturbation
procedure :: field_momentumfluxq ! q_i = T^0_i = \rho u_i = (\rho + P)*(v_i - B_i)
procedure :: field_comovingsoundspeed
! Initializing and getting parameters
procedure :: field_getpotentialparams
procedure :: field_initialvalues
! Evolving field
procedure :: field_backgroundevolve
procedure :: field_perturbationevolve

contains

subroutine field_getpotentialparams(quintessence, Ini) ! Gets the potential form and parameters from the .ini file
use IniObjects
type(TQuintessence_Joao), intent(inout) :: quintessence
type(TIniFile), intent(in) :: Ini

quintessence%potential_type = Ini%Read_Int('field_potential_type', 1)
quintessence%potentialparams(1) = Ini%Read_Double('potentialparam1', 0.d0)
quintessence%potentialparams(2) = Ini%Read_Double('potentialparam2', 0.d0)
quintessence%potentialparams(3) = Ini%Read_Double('potentialparam3', 0.d0)

end subroutine field_getpotentialparams

function field_potential(phi, potentialparams) ! This function returns an array with V(phi), V'(phi) and V''(phi), necessary quantities to the equations. Manage units!!

	real(dl), dimension(3) :: field_potential
	real(dl) :: phi
	
	select case (n)
	case (1)
		field_potential(1) = ... ! Fill here for V(phi) and its derivatives
		field_potential(2) = ...
		field_potential(3) = ...
	case (2)
		field_potential(1) = ...
		field_potential(2) = ...
		field_potential(3) = ...
	end select
	
end function field_potential

function field_kineticenergy(phidot, a)

	real(dl) :: field_kineticenergy, phidot, a
	field_kineticenergy = phidot**2/(2*a**2)
	
end function field_kineticenergy

function field_totalenergy(phi, phidot, a)

	real(dl) :: phi, phidot, a, field_totalenergy
	field_totalenergy = field_potential(phi, 0) + field_kineticenergy(phidot, a)
	
end function field_totalenergy

function field_eos(phi, phidot, a)
	
	real(dl) :: phi, phidot, a, field_eos
	field_eos = (field_potential(phi, 0) - field_kineticenergy(phidot, a))/field_totalenergy(phi, phidot, a)
	
end function field_eos

function field_densityperturbation(a, phi, phidot, dphi, dphidot)

	real(dl) :: a, phi, phidot, dphi, dphidot, field_densityperturbation
	field_densityperturbation = phidot*dphidot/a**2 + field_potential(phi, 1)*dphi
	
end function field_densityperturbation

function field_pressureperturbation(a, phi, phidot, dphi, dphidot)

	real(dl) :: a, phi, phidot, dphi, dphidot, field_pressureperturbation
	field_pressureperturbation = phidot*dphidot/a**2 - field_potential(phi, 1)*dphi
	
end function field_densityperturbation

function field_momentumfluxq(a, k, phidot, dphi)

	real(dl) :: a, k, phidot, dphi, field_momentumfluxq
	field_momentumfluxq = k*phidot*dphi/a**2

end function field_momentumfluxq

real(dl) function field_comovingsoundspeed()
	field_comovingsoundspeed = 1
end function field_comovingsoundspeed

subroutine field_initialvalues(quintessence)

	type(TQuintessence_Joao), intent(in) :: quintessence
	
end subroutine field_initialvalues

subroutine field_backgroundevolve(a, phi, phidot, derivs) ! Need to include quantities from other components, this can be done through state

	! derivs is the derivatives vector for the system (phi, phidot), so that derivs(1) = phidot, derivs(2) = -2*H*phidot - a**2 * V'(phi) but I need to change to a
	real(dl), intent(in) :: a, phi, phidot
	real(dl), intent(out) :: derivs(2)
	
	derivs(1) = phidot
	derivs(2) = ...
	
end subroutine field_backgroundevolve

subroutine field_perturbationevolve(a, phi, phidot, dphi, dphidot, derivs) ! Need to include quantities from other components, this can be done through state

	real(dl), intent(in) :: a, phi, phidot, dphi, dphidot
	real(dl), intent(out) :: derivs(2)
	
	derivs(1) = dphidot
	derivs(2) = ...
	
end subroutine field_perturbationevolve
