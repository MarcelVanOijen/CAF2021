module belowgroundres

use declare_parameters
use environment
use management
use shade
use scaling
implicit none

Contains

  Subroutine PET(LAI,Rainint,T,GR, Pevap,Ptran)
  !=============================================================================
  ! Calculate potential rates of evaporation and transpiration (mm d-1)
  ! Author - Marcel van Oijen
  !=============================================================================
  real :: LAI(nc), Rainint(nc), T(nc), GR(nc)
  real :: Pevap(nc), Ptran(nc)
  real :: BOLTZM, LHVAP, PSYCH, WDF
  real :: BBRAD(nc), NRADC(nc), NRADS(nc)
  real :: PENMD(nc), PENMRC(nc), PENMRS(nc)
  real :: RLWN(nc), SSLOPE(nc), SVP(nc)
  
  BOLTZM = 5.668E-8                                      ! % (J m-2 s-1 K-4)
  LHVAP  = 2.4E6                                         ! % (J kg-1)
  PSYCH  = 0.067                                         ! % (kPA degC-1))
  BBRAD  = BOLTZM * (T+273.)**4 * 86400.                 ! % (J m-2 d-1)
  SVP    = 0.611 * exp(17.4 * T/(T+239.))                ! % (kPa)
  SSLOPE = 4158.6 * SVP / (T+239.)**2                    ! % (kPA degC-1)
  RLWN   = BBRAD * max(0.,0.55*(1.-VP/SVP))              ! % (J m-2 d-1)
  NRADS  = GR*1.E6 * (1.-0.15) - RLWN                    ! % (J m-2 d-1)
  NRADC  = GR*1.E6 * (1.-0.25) - RLWN                    ! % (J m-2 d-1)
  PENMRS = NRADS * SSLOPE/(SSLOPE+PSYCH)                 ! % (J m-2 d-1)
  PENMRC = NRADC * SSLOPE/(SSLOPE+PSYCH)                 ! % (J m-2 d-1)
  WDF    = 2.63 * (1.0 + 0.54 * WN)                      ! % (kg m-2 d-1 kPa-1)
  PENMD  = LHVAP * WDF * (SVP-VP) * PSYCH/(SSLOPE+PSYCH) ! % (J m-2 d-1)

  Pevap  =     exp(-0.5*LAI)  * (PENMRS + PENMD) / LHVAP ! % (mm d-1)
  Ptran  = (1.-exp(-0.5*LAI)) * (PENMRC + PENMD) / LHVAP ! % (mm d-1)
  Ptran  = max( 0., Ptran-0.5*Rainint )                  ! % (mm d-1)

  end Subroutine PET

  Subroutine water_flux(Pevap,Ptran,TRANCO,WA, Evap,fTran,RWA,Tran)
  !=============================================================================
  ! Calculate rates of evaporation and transpiration (mm d-1), and the
  ! transpiration realisation factor (-)
  ! Author: Marcel van Oijen
  ! Date  : 6-11-2005, Modified: 27-3-2020
  !=============================================================================
  real :: Pevap(nc), Ptran(nc), TRANCO, WA(nc)
  real :: Evap(nc), fTran(nc), RWA(nc), Tran(nc)
  real :: WAAD, WCAD, WCFC, WCWP, WCWET
  real :: fAvail(nc), FRR(nc), RWAevap(nc), WC(nc), WCCR(nc)
  WCAD    = WCST * FWCAD                                           ! % (m3 m-3)
  WCWP    = WCST * FWCWP                                           ! % (m3 m-3)
  WCFC    = WCST * FWCFC                                           ! % (m3 m-3)
  WCWET   = WCST * FWCWET                                          ! % (m3 m-3)
  WC      = 0.001 * WA   / ROOTD                                   ! % (m3 m-3)
  RWA     = max(0., min(1., (WC - WCWP) / (WCFC - WCWP) ) )        ! % (-)
  RWAevap = max(0., min(1., (WC - WCAD) / (WCFC - WCAD) ) )        ! % (-)
  WAAD    = 1000. * WCAD * ROOTD                                   ! % (mm)
  Evap    = Pevap * RWAevap                                        ! % (mm d-1)
  WCCR    = WCWP + max( 0.01, Ptran/(Ptran+TRANCO) * (WCFC-WCWP) ) ! % (m3 m-3)
  where (WC>WCCR)
    FRR = max(0., min(1., (WCST-WC)/(WCST-WCWET) ))
  elsewhere
    FRR = max(0., min(1., (WC-WCWP)/(WCCR-WCWP)  ))
  endwhere                                                         ! % (mm mm-1)
  Tran    = Ptran * FRR                                            ! % (mm d-1)
  where ((Evap+Tran)>0)
    fAvail = min( 1., ((WA-WAAD)/DELT) / (Evap+Tran) )
  elsewhere
    fAvail = 0.                                                
  endwhere                                                         ! % (mm mm-1)
  Evap    = Evap * fAvail                                          ! % (mm d-1)
  Tran    = Tran * fAvail                                          ! % (mm d-1)
  where (Ptran>0)
    fTran = Tran / Ptran                                           ! % (-)
  elsewhere
    fTran = 1.                                                     ! % (-)
  endwhere
  end Subroutine water_flux
  
  Subroutine Nsupply(CR,NMIN, Nsup)
  real :: CR(nc), NMIN(nc)
  real :: Nsup(nc)
  Nsup = min( CR * KNUPT * NMIN / (KNMIN+NMIN), NMIN/DELT )
  end Subroutine Nsupply

  Subroutine Nsupplytree(At,Atc,CRT_t,NMIN, NsupT_t)
  real    :: At(nt), Atc(nt,nc), CRT_t(nt), NMIN(nc)
  real    :: NsupT_t(nt)
  real    :: CRT_tc(nt,nc), DUMMY_c(nc), NsupT_tc(nt,nc)=0  
  integer :: it
  call RescaleExt_t_tcc(CRT_t,At,Atc, CRT_tc,DUMMY_c)
  do it=1,nt
    NsupT_tc(it,:) = min( CRT_tc(it,:) * KNUPTT * NMIN / (KNMINT+NMIN), NMIN/DELT )
    NsupT_t (it  ) = sum( Atc(it,:) * NsupT_tc(it,:) )
  enddo
  end Subroutine Nsupplytree
  
end module belowgroundres      
