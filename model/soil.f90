module soil

use declare_parameters
use environment
use management
implicit none

real :: dCLITT(nc), rCLITT(nc), rCSOMF(nc), Rsoil(nc)
real :: dCLITTsomf(nc), dCSOMF(nc), dCSOMFsoms(nc), dCSOMS(nc)
real :: Nemission(nc), Nleaching(nc)
real :: NLITTsomf(nc), Nmineralisation(nc)
real :: dNLITT(nc), dNSOMF(nc), dNSOMS(nc), NSOMFsoms(nc), rNLITT(nc), rNSOMF(nc)
real :: Tsoil, fTsoil, Thist(10)
real :: RUNOFF(nc), Drain(nc)

Contains

  Subroutine water(WA,Rainint,Evap,Tran,LAI,RUNOFF,Drain)
  !=============================================================================
  ! Calculate rates of runoff and drainage (mm d-1)
  ! Author - Marcel van Oijen
  ! 29-03-2020
  !=============================================================================
  integer :: day
  real    :: Evap(:), LAI(:), Rainint(:), Tran(:), WA(:)
  real    :: RUNOFF(:), Drain(:)
  real    :: WAFC, WCFC
  WCFC   = WCST * FWCFC                                     ! % (m3 m-3)
  WAFC   = 1000. * WCFC * ROOTD                             ! % (mm)
  RUNOFF = (RAIN-Rainint) * sin(atan(SLOPE/100)) * &
                                        exp(-KRUNOFF * LAI) ! % (mm d-1)
  Drain  = max(0. , (WA-WAFC)/DELT + &
                    RAIN - RAINint - Runoff - Evap - Tran ) ! % (mm d-1)
  end Subroutine water

  Subroutine CNsoil(RWA,WA,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS)
  real :: CLITT(:), CSOMF(:), CSOMS(:), NLITT(:), NMIN(:), NSOMF(:), NSOMS(:)
  real :: RWA(:), WA(:)
  real :: dCLITTrsoil(nc), dCSOMFrsoil(nc), NLITTnmin(nc), NSOMFnmin(nc)
  fTsoil = 1
  ! C Litter
  rCLITT          = (CLITT * Runoff / ROOTD) * RRUNBULK * 0.001
  dCLITT          =  CLITT * fTsoil / TCLITT
  dCLITTsomf      = FLITTSOMF * dCLITT
  dCLITTrsoil     = dCLITT - dCLITTsomf
  ! C SOM fast
  rCSOMF          = (CSOMF * Runoff / ROOTD) * RRUNBULK * 0.001
  dCSOMF          =  CSOMF * fTsoil / TCSOMF
  dCSOMFsoms      = FSOMFSOMS * dCSOMF
  dCSOMFrsoil     = dCSOMF - dCSOMFSOMS
  ! C SOM slow
  dCSOMS          = CSOMS * fTsoil / TCSOMS
  ! Respiration
  Rsoil           = dCLITTrsoil + dCSOMFrsoil + dCSOMS
  ! N Litter
  rNLITT          = (NLITT * Runoff / ROOTD) * RRUNBULK * 0.001
  dNLITT          =  NLITT * dCLITT / CLITT
  NLITTsomf       = dNLITT * FLITTSOMF
  NLITTnmin       = dNLITT - NLITTsomf
  ! N SOM fast
  rNSOMF          = (NSOMF * Runoff / ROOTD) * RRUNBULK * 0.001
  dNSOMF          =  NSOMF * dCSOMF / CSOMF
  NSOMFsoms       = dNSOMF * FSOMFSOMS
  NSOMFnmin       = dNSOMF - NSOMFsoms
  ! N SOM slow
  dNSOMS          = NSOMS * dCSOMS / CSOMS
  ! N mineralisation, leaching, emission
  Nmineralisation = NLITTnmin + NSOMFnmin + dNSOMS
  Nleaching       = NMIN * RNLEACH * Drain / WA
  Nemission       = NMIN * KNEMIT  * RWA
  end Subroutine CNsoil

end module soil      
