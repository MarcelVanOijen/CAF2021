subroutine abovegroundres(LAI,PARCOFFEE,PARav,PARint)
use declare_parameters
use environment
implicit none
real :: LAI(nc), PARCOFFEE(nc), PARav(nc), PARint(nc)
PARav  = PARCOFFEE / DAYL
PARint = PARCOFFEE * (1. - exp(-KEXT*LAI))
end Subroutine abovegroundres
