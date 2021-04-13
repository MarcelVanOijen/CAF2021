Subroutine CAF2021(PARAMS,MATRIX_WEATHER, &
                   CALENDAR_FERT,CALENDAR_PRUNC,CALENDAR_PRUNT,CALENDAR_THINT, &
				   NDAYS,NOUT, &
				   y)
!========================================================
! This is the CAF2021 model.
! Authors: Marcel van Oijen, David Cameron, Oriana Ovalle
! Date: 2021-04-12
!========================================================

use belowgroundres
use coffee
use declare_parameters
use environment
use management
use shade
use scaling
use soil
use tree
implicit none

! As long as the total number of parameters stays below 120, the next line need not be changed
integer, parameter :: NPAR     = 130
real               :: PARAMS(NPAR)
integer, parameter :: NWEATHER = 8
real               :: MATRIX_WEATHER(NMAXDAYS,NWEATHER)

! real   , dimension(100,3) :: CALENDAR_FERT, CALENDAR_PRUNC, CALENDAR_PRUNT, CALENDAR_THINT
! integer, dimension(100,2) :: DAYS_FERT    , DAYS_PRUNC    , DAYS_PRUNT    , DAYS_THINT
! real   , dimension(100)   ::                FRPRUNC       , FRPRUNT       , FRTHINT
real   , dimension(100,3)   :: CALENDAR_FERT , CALENDAR_PRUNC
integer, dimension(100,2)   :: DAYS_FERT     , DAYS_PRUNC
real   , dimension(100)     ::                 FRPRUNC
real   , dimension(3,100,3) :: CALENDAR_PRUNT, CALENDAR_THINT
integer, dimension(3,100,2) :: DAYS_PRUNT    , DAYS_THINT
real   , dimension(3,100)   :: FRPRUNT       , FRTHINT

integer, dimension(100)     :: NFERTV
real                        :: y(NDAYS,NOUT)

integer :: day, doy, i, ic, it, NDAYS, NOUT, year

! State variables
real    :: CBT_t(nt), CLT_t(nt), CRT_t(nt), CST_t(nt), NLT_t(nt)
real    :: CL(nc), CP(nc), CR(nc), CW(nc), NL(nc), NCL(nc)
real    :: daysinceprun, DVS(nc), LAI(nc), SENSIT(nc), SINKP(nc)
real    :: CLITT(nc), CSOMF(nc), CSOMS(nc)
real    :: NLITT(nc), NMIN (nc), NSOMF(nc), NSOMS(nc), WA(nc)

! Non-state variables
real    :: T_c(nc), GR_c(nc)

real    :: Ac(nc), At(nt), Atc(nt,nc)

real    :: DUMMY_c(nc), DUMMY_tc(nt,nc)
real    :: fTranT_c(nc)=0, fTranT_t(nt)=0
real    :: LAIT, LAIT_c(nc)=0, LAIT_t(nt)=0, LAIT_tc(nt,nc)=0
real    :: NsupT_t(nt)=0
real    :: Pevap(nc), PevapT_c(nc), Ptran(nc), PtranT_c(nc)
real    :: RainintT_c(nc)=0, TranT_c(nc)=0
real    :: fNgrowth(nc)
real    :: Evap(nc) , fTran    (nc), Nsup(nc)   , Tran(nc) , RWA(nc)
real    :: PARav(nc), PARint   (nc), Rainint(nc), TCOFFEE(nc)
real    :: PARMA(nc), PARCOFFEE(nc), PARold(nc,30)
real    :: harvDMav
real    :: dCLT_c (nc)=0, dCBT_c   (nc)=0, dCRT_c   (nc)=0
real    :: dNLT_c (nc)=0, dNBlitt_c(nc)=0, dNRsomf_c(nc)=0
real    :: NuptT_c(nc)=0, NfixT_c  (nc)=0

! EXTRA OUTPUT VARIABLES
real 	  :: Cabg, harvDMav_year, LAI_f
real 	  :: CabgT
real    :: Csoil(nc), Csoil_f, Nsoil(nc), Nsoil_f, WA_f, WC_f

! PARAMETERS
call set_params(PARAMS)

! Calendar of weather
YEARI = MATRIX_WEATHER(:,1)
DOYI  = MATRIX_WEATHER(:,2)
GRI   = MATRIX_WEATHER(:,3)
TMINI = MATRIX_WEATHER(:,4)
TMAXI = MATRIX_WEATHER(:,5)
VPI   = MATRIX_WEATHER(:,6)
WNI   = MATRIX_WEATHER(:,7)
RAINI = MATRIX_WEATHER(:,8)

! Calendar of management
DAYS_FERT    = CALENDAR_FERT (:,1:2)
DAYS_PRUNC   = CALENDAR_PRUNC(:,1:2)
NFERTV       = CALENDAR_FERT (:,3)
FRPRUNC      = CALENDAR_PRUNC(:,3)

! DAYS_PRUNT   = CALENDAR_PRUNT(:,1:2)
! DAYS_THINT   = CALENDAR_THINT(:,1:2)
! FRPRUNT      = CALENDAR_PRUNT(:,3)
! FRTHINT      = CALENDAR_THINT(:,3)
DAYS_PRUNT   = CALENDAR_PRUNT(:,:,1:2)
DAYS_THINT   = CALENDAR_THINT(:,:,1:2)
FRPRUNT      = CALENDAR_PRUNT(:,:,3)
FRTHINT      = CALENDAR_THINT(:,:,3)

daysinceprun = 0

! INITIAL STATES

! Trees
treedens_t = TREEDENS0
CBT_t      = CBtree0 * treedens_t
CLT_t      = CLtree0 * treedens_t
CRT_t      = CRtree0 * treedens_t
CST_t      = CStree0 * treedens_t
NLT_t      = CLtree0 * treedens_t * NCLMAXT
LAIT_t     = CLT_t * SLAT

! Agroforestry system
call calcTX
CAtree_t       = KAC * (CBtree0**KACEXP)
! SAT_t          = min(1., CAtree_t * treedens_t * SHADEPROJ)
! SAT_t(1:ntlow) = SAT_t(1:ntlow) / max(1., sum(SAT_t(1:ntlow)))
SAT_t          = CAtree_t * treedens_t * SHADEPROJ
SAT_t(1:ntlow) = SAT_t(1:ntlow) / max(1., sum(SAT_t(1:ntlow)))
SAT_t(3)       = min(1., SAT_t(3))
Ac(5:6)        = SAT_t(3) * SAT_t(1:2)
Ac(4)          = SAT_t(3) * (1-SAT_t(1)-SAT_t(2))
Ac(2:3)        = (1.-SAT_t(3)) * SAT_t(1:2)
Ac(1)          = max( 0., 1. - sum(Ac(2:nc)) )

! Soil
CLITT = CLITT0
CSOMF = CSOM0 * FCSOMF0
CSOMS = CSOM0 * (1-FCSOMF0)
NLITT = CLITT0 / CNLITT0
NSOMF = CSOM0 *    FCSOMF0  / CNSOMF0
NSOMS = CSOM0 * (1-FCSOMF0) / CNSOMS0
NMIN  = NMIN0
WA    = 1000 * ROOTD * WCST * FWCFC

! Coffee
DVS           = DVS0
CL            = CL0
CP            = CP0
CR            = CR0
CW            = CW0
NL            = CL * NCLMAX
LAI           = CL * SLAMAX
SENSIT        = 0
SINKP         = 0
SINKPMAXnew   = FSINKPMAX0 * SINKPMAX
PARold        = 0
harvDMav_year = 0

do day = 1, NDAYS

! Environment
  call set_weather_day(day, year,doy)
  T_c  = T
  GR_c = GR
  
! Trees
  call morphology(CBT_t,CST_t,LAIT_t,treedens_t, SAT_t)

! Management
  call fert_prune_thin(year,doy,DAYS_FERT ,NFERTV ,DAYS_PRUNC,FRPRUNC, &
                                DAYS_PRUNT,FRPRUNT,DAYS_THINT,FRTHINT)
  treedens_t = treedens_t - thintreedens_t
  
! Land-cover areas
  call CalcShade(LAI,CR,CW,CP,CL,NL,WA,CLITT,CSOMF,CSOMS,NLITT,NSOMF,NSOMS,NMIN, Ac)
  call CalcAtc(Ac,TX, At,Atc)
  call RescaleExt_t_tcc(LAIT_t,At,Atc, LAIT_tc,LAIT_c)
  
! Trees continued
  RainintT_c = min( RAIN, KRAININTT*LAIT_c )
  call PET(LAIT_c,RainintT_c,T_c,GR_c, PevapT_c,PtranT_c)
  call water_flux(PevapT_c,PtranT_c,TRANCOT,WA, &
                  DUMMY_c,fTranT_c,DUMMY_c,TranT_c)
  call RescaleInt_c_t( fTranT_c,At,Atc, fTranT_t )
  call PARintT(Ac,Atc,LAIT_tc, PARintT_c,PARintT_t)
  call NPP(fTranT_t,PARintT_t)
  
  call Nsupplytree(At,Atc,CRT_t,NMIN, NsupT_t)
  call allocation(At,CLT_t,fTranT_t,LAIT_t,NLT_t)
  call NdemandOrgans
  call gtreeNupt(NsupT_t)
  call CNtree(fTranT_t,CRT_t,CST_t,CBT_t,CLT_t,NLT_t)
  
! Environment continued
  call DDAYL(doy)
  
! Coffee
  PARCOFFEE      = PAR - PARintT_c
  TCOFFEE        = T   - TDIFFMAX * (1. - PARCOFFEE/PAR)
  Rainint        = min( RAIN-RainintT_c, KRAININT*LAI )                     
  PARMA          = sum( PARold,2 ) / 30.
  PARold(:,2:30) = PARold(:,1:29)
  PARold(:,1   ) = PARCOFFEE
  call PET(LAI,Rainint,TCOFFEE,2*PARCOFFEE, Pevap,Ptran)
  call water_flux(Pevap,Ptran,TRANCO,WA, Evap,fTran,RWA,Tran)
  call Nsupply(CR,NMIN,Nsup)
  call abovegroundres(LAI,PARCOFFEE,PARav,PARint)
  call Phenology(Day,doy,DVS,SENSIT,TCOFFEE,SINKP)
  call growth(TCOFFEE,PARav,PARint,daysinceprun,fTran,SINKP,Nsup,PARMA,DVS,fNgrowth)
  call Foliage(fTran)  
  call Senescence(CR,NL,CL,LAI,fTran)
  call PrunHarv(NL,CL,CW,CP,LAI,DVS)
	
! Soil continued
  call water(WA,Rainint+RainintT_c,Evap,Tran+TranT_c,LAI+LAIT_c,RUNOFF,Drain)
  call CNsoil(RWA,WA,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS) 

! STATE EQUATIONS

! Coffee (arrays: [m-2 sun, m-2 shade])
  where(DVS<1.)
    SINKP = SINKP + gsink
  elsewhere
    SINKP=0
  endwhere
  ! Sun and shade coffee plants pruned at the same time
  daysinceprun = max( 0., daysinceprun + PRUN - 1/DAYSPRNOP )
  LAI    = LAI    + adjLAI  + gLAI - dLAI - prunLAI
  NL     = NL     + adjNL   + gNL  - dNL  - prunNL
  CL     = CL     + adjCL   + gCL  - dCL  - prunCL  
  CW     = CW     + adjCW   + gCW         - prunCW
  CR     = CR     + adjCR   + gCR  - dCR
  CP     = CP     + adjCP   + gCP         - harvCP
  SENSIT = SENSIT + dSENSIT
  DVS    = DVS    + dDVS    - rDVS

! Trees (scalars: m-2 field)
  where (At>0.)
    LAIT_t = CLT_t * SLAT
    CBT_t  = CBT_t + gCBT_t - dCBT_t
    CLT_t  = CLT_t + gCLT_t - dCLT_t
    CRT_t  = CRT_t + gCRT_t - dCRT_t
    CST_t  = CST_t + gCST_t - dCST_t
    NLT_t  = NLT_t + gNLT_t - dNLT_t
  endwhere
  
! Splitting tree field-fluxes over different land cover classes
  call RescaleExt_t_tcc(dCLT_t   ,At,Atc, DUMMY_tc,dCLT_c   )
  call RescaleExt_t_tcc(dCBT_t   ,At,Atc, DUMMY_tc,dCBT_c   )
  call RescaleExt_t_tcc(dCRT_t   ,At,Atc, DUMMY_tc,dCRT_c   )
  call RescaleExt_t_tcc(dNLT_t   ,At,Atc, DUMMY_tc,dNLT_c   )
  call RescaleExt_t_tcc(dNBlitt_t,At,Atc, DUMMY_tc,dNBlitt_c)
  call RescaleExt_t_tcc(dNRsomf_t,At,Atc, DUMMY_tc,dNRsomf_c)
  call RescaleExt_t_tcc(NfixT_t  ,At,Atc, DUMMY_tc,NfixT_c  )
  call RescaleExt_t_tcc(NuptT_t  ,At,Atc, DUMMY_tc,NuptT_c  )

! Soil (arrays: [m-2 sun, m-2 shade])
  WA    = WA    + adjWA    + RAIN       - Rainint   - RainintT_c - Runoff &
                - Drain    - Evap       - Tran      - TranT_c
  CLITT = CLITT + adjCLITT + dCL        + dCLT_c    + dCBT_c &
                - rCLITT   - dCLITT     + prunCL    + prunCW
  CSOMF = CSOMF + adjCSOMF + dCLITTsomf + dCR       + dCRT_c - rCSOMF - dCSOMF
  CSOMS = CSOMS + adjCSOMS + dCSOMFsoms - dCSOMS
  NLITT = NLITT + adjNLITT + dNLT_c     + dNBlitt_c + dNL   + prunNL &
                + prunCW*NCW - rNLITT   - dNLITT
  NSOMF = NSOMF + adjNSOMF + dCR*NCR    + dNRsomf_c + NLITTsomf - rNSOMF - dNSOMF
  NSOMS = NSOMS + adjNSOMS + NSOMFsoms  - dNSOMS
  NMIN  = NMIN  + adjNMIN  + Nfert      + Nmineralisation &
                + NfixT_c  - Nupt       - NuptT_c   - Nleaching - Nemission
				
! Additional output variables
  Cabg     = sum(Ac*(CL+CW+CP))            ! kgC m-2 field
  CabgT    = sum(CLT_t + CST_t + CBT_t)    ! kgC m-2 field
  Csoil    = CLITT + CSOMF + CSOMS         ! kgC m-2 c
  Csoil_f  = sum(Ac*Csoil) * 10            ! tC  ha-1 field
  harvDMav = sum(Ac*harvCP) * 10./CCONC    ! tDM ha-1 field
  if (doy==61) then
	harvDMav_year = 0
  else
	harvDMav_year = harvDMav_year + harvDMav ! tDM ha-1
  endif
  LAI_f    = sum(Ac*LAI)                   ! m2  m-2 field
  LAIT     = sum(LAIT_t)/sum(At)           ! m2  m-2 shade
  Nsoil    = NLITT + NSOMF + NSOMS + NMIN  ! kgN m-2 c
  Nsoil_f  = sum(Ac*Nsoil) * 10            ! tN  ha-1 field
  WA_f     = sum(Ac*WA)                    ! kgW m-2 field
  WC_f     = 0.001 * WA_f / ROOTD   	   ! m3W m-3

! Outputs
! The "c1", "c2" etc. in the units below refer to parts of the field with
! specific combinations of shade tree species:
! c1 = sun (no tree species at all)
! c2 = shaded by tree sp. 1     (lower-stratum tree sp.)
! c3 = shaded by tree sp. 2     (lower-stratum tree sp.)
! c4 = shaded by tree sp. 3     (upper-stratum tree sp.)
! c5 = shaded by tree sp. 1 & 3 (lower- and upper-stratum tree spp.)
! c6 = shaded by tree sp. 2 & 3 (lower- and upper-stratum tree spp.)
  y(day, 1) = year + (doy-0.5)/366         ! "Time" = Decimal year (approx.)
  y(day, 2) = year
  y(day, 3) = doy

  y(day, 4) = Ac(1)         ! m2 c m-2
  y(day, 5) = Ac(2)         ! m2 c m-2
  y(day, 6) = Ac(3)         ! m2 c m-2
  y(day, 7) = Ac(4)         ! m2 c m-2
  y(day, 8) = Ac(5)         ! m2 c m-2
  y(day, 9) = Ac(6)         ! m2 c m-2
  y(day,10) = At(1)         ! m2 t m-2
  y(day,11) = At(2)         ! m2 t m-2
  y(day,12) = At(3)         ! m2 t m-2
  y(day,13) = fNgrowth(1)   ! -
  y(day,14) = fNgrowth(2)   ! -
  y(day,15) = fNgrowth(3)   ! -
  y(day,16) = fNgrowth(4)   ! -
  y(day,17) = fNgrowth(5)   ! -
  y(day,18) = fNgrowth(6)   ! -
  y(day,19) = fTran(1)      ! -
  y(day,20) = fTran(2)      ! -
  y(day,21) = fTran(3)      ! - 
  y(day,22) = fTran(4)      ! -
  y(day,23) = fTran(5)      ! -
  y(day,24) = fTran(6)      ! -

  y(day,25) = Cabg  		! kgC m-2
  y(day,26) = harvCP(1)     ! kgC m-2 c
  y(day,27) = harvCP(2)     ! kgC m-2 c
  y(day,28) = harvCP(3)     ! kgC m-2 c
  y(day,29) = harvCP(4)     ! kgC m-2 c
  y(day,30) = harvCP(5)     ! kgC m-2 c
  y(day,31) = harvCP(6)     ! kgC m-2 c
  y(day,32) = harvDMav_year ! tDM ha-1
  y(day,33) = LAI(1)        ! m2 m-2 c
  y(day,34) = LAI(2)        ! m2 m-2 c
  y(day,35) = LAI(3)        ! m2 m-2 c
  y(day,36) = LAI(4)        ! m2 m-2 c
  y(day,37) = LAI(5)        ! m2 m-2 c
  y(day,38) = LAI(6)        ! m2 m-2 c
  
  y(day,39) = CabgT   		! kgC m-2
  y(day,40) = CAtree_t(1)   ! m2 tree-1
  y(day,41) = CAtree_t(2)   ! m2 tree-1
  y(day,42) = CAtree_t(3)   ! m2 tree-1
  y(day,43) = h_t(1)        ! m
  y(day,44) = h_t(2)        ! m
  y(day,45) = h_t(3)        ! m
  y(day,46) = LAIT_c(1)     ! m2 m-2 c
  y(day,47) = LAIT_c(2)     ! m2 m-2 c
  y(day,48) = LAIT_c(3)     ! m2 m-2 c
  y(day,49) = LAIT_c(4)     ! m2 m-2 c
  y(day,50) = LAIT_c(5)     ! m2 m-2 c
  y(day,51) = LAIT_c(6)     ! m2 m-2 c
  y(day,52) = treedens_t(1) ! m-2
  y(day,53) = treedens_t(2) ! m-2
  y(day,54) = treedens_t(3) ! m-2
  
  y(day,55) = Csoil_f       ! tC ha-1
  y(day,56) = Nsoil_f       ! tN ha-1
  
  y(day,57) = CST_t(1)      ! kgC m-2
  y(day,58) = CST_t(2)      ! kgC m-2
  y(day,59) = CST_t(3)      ! kgC m-2
  y(day,60:62) = SAT_t      ! m2 m-2

! CALIBRATION VARIABLES IN BC DATA FILES.
! NAME IN CAF2021 ! NAME IN BC data files ! Unit
! (this version)  ! (if different)        !
! -----------------------------------------------------------
! 1.-Ac(1)        ! SA                    ! m2 shade m-2  field
! Cabg            ! CT                    ! kg C     m-2  field
! CabgT           ! CTT                   ! kg C     m-2  field
! CAtree_t(1)     ! CAtree                ! m2       tree-1
! CL(1)           !                       ! kg C     m-2  c1
! CL(2)           !                       ! kg C     m-2  c2
! Csoil_f         ! Csoilave              ! t  C     ha-1 field
! CW(1)           !                       ! kg C     m-2  c1
! CW(2)           !                       ! kg C     m-2  c2
! h_t(1)          ! h                     ! m
! harvCP(1)       !                       ! kg C     m-2  c1
! harvCP(2)       !                       ! kg C     m-2  c2
! harvDMav_year   !                       ! t  DM    ha-1 field
! LAI_f           ! LAIave                ! m2       m-2  field
! LAI(1)          !                       ! m2       m-2  c1
! LAI(2)          !                       ! m2       m-2  c2
! LAIT            !                       ! m2       m-2  shade
! Nsoil_f         ! Nsoilave              ! t  N     ha-1 field
! WC_f            ! WC_F                  ! m3 W     m-3  field

! if(day==1) then
!	  write(66,*) "day=1    : PARCOFFEE= ", PARCOFFEE
!	  write(66,*) "day=1    : treedens_t=", treedens_t
!	  write(66,*) "day=1    : CLT_t=     ", CLT_t
!	  write(66,*) "day=1    : gCLT_t=    ", gCLT_t
! endif
! if(day==NDAYS) then
!   write(66,*) "---------------------------------------------------"
!	  write(66,*) "day=NDAYS: PARCOFFEE= ", PARCOFFEE
!	  write(66,*) "day=NDAYS: treedens_t=", treedens_t
!	  write(66,*) "day=NDAYS: CLT_t=     ", CLT_t
!	  write(66,*) "day=NDAYS: gCLT_t=    ", gCLT_t
! endif

end do ! end time loop

! close(66)

end  
