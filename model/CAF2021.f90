Subroutine CAF2021(PARAMS,MATRIX_WEATHER, &
                   CALENDAR_FERT,CALENDAR_PRUNC,CALENDAR_PRUNT,CALENDAR_THINT, &
				   NDAYS,NOUT, &
				   y)
!========================================================
! This is the CAF2021 model.
! Authors: Marcel van Oijen, David Cameron, Oriana Ovalle
! Date: 2021-05-31
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

! As long as the total number of parameters stays below 160, the next line need not be changed
integer, parameter :: NPAR = 160
real               :: PARAMS(NPAR)
integer, parameter :: NWEATHER = 8
real               :: MATRIX_WEATHER(NMAXDAYS,NWEATHER)

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
real :: CBT_t(nt)  , CLT_t(nt)  , CPT_t (nt)  , CRT_t(nt)  , CST_t(nt), NLT_t(nt)
real :: CL   (nc)=0, CP   (nc)=0, CR    (nc)=0, CW   (nc)=0, NL   (nc)=0
real :: DVS  (nc)=0, LAI  (nc)=0, SENSIT      , SINKP(nc)
real :: CLITT(nc)  , CSOMF(nc)  , CSOMS (nc)
real :: NLITT(nc)  , NMIN (nc)  , NSOMF (nc)  , NSOMS(nc)  , WA   (nc)

! Non-state variables
real :: T_c(nc), GR_c(nc)

real :: Ac(nc), At(nt), Atc(nt,nc)

real :: DUMMY_c(nc), DUMMY_tc(nt,nc)
real :: fTranT_c(nc)=0, fTranT_t(nt)=0
real :: LAIT, LAIT_c(nc)=0, LAIT_t(nt)=0, LAIT_tc(nt,nc)=0
real :: NsupT_t(nt)=0
real :: Pevap(nc), PevapT_c(nc), Ptran(nc), PtranT_c(nc)
real :: RainintT_c(nc)=0, TranT_c(nc)=0
real :: fNgrowth(nc)
real :: Evap(nc) , fTran    (nc), Nsup(nc)   , Tran(nc) , RWA(nc)
real :: PARav(nc), PARint   (nc), Rainint(nc), TCOFFEE(nc)
real :: PARMA(nc), PARCOFFEE(nc), PARold(nc,30)
real :: harvDM_f_ha
real :: dCLTlitt_c(nc)=0, dCBTlitt_c(nc)=0, dCRT_c(nc)=0
real :: sCSTsen_c(nc)=0
real :: dNLTlitt_c(nc)=0, dNBTlitt_c(nc)=0, dNSTlitt_c(nc)=0, dNRsomf_c(nc)=0
real :: NuptT_c(nc)=0, NfixT_c(nc)=0

! EXTRA OUTPUT VARIABLES
real :: Cabg_f     , harvDM_f_hay, LAI_f
real :: CabgT_f    , C           , CT
real :: Csoil(nc)  , Csoil_f     , Nsoil(nc)  , Nsoil_f   , WA_f   , WC_f
real :: Nfert_f    , NfixT_f     , NsenprunT_f, Nsenprun_f
real :: Nleaching_f, Nemission_f , Nrunoff_f  , Nupt_f    , NuptT_f
real :: CsenprunT_f, Csenprun_f  , Rsoil_f    , Crunoff_f
real :: Rain_f     , Drain_f     , Runoff_f   , Evap_f
real :: Tran_f     , TranT_f     , Rainint_f  , RainintT_f
real :: C_f        , gC_f        , dC_f       , prunC_f   , harvCP_f
real :: CT_f       , gCT_f       , harvCBT_f  , harvCPT_f , harvCST_f
real :: CR_f       , CW_f        , CL_f       , CP_f

real ::   Csoil_f1   =0,   Csys_f1   =0,   Nsoil_f1   =0
real :: D_Csoil_f_hay=0, D_Csys_f_hay=0, D_Nsoil_f_hay=0

real :: NfixT_f_hay=0, Nleaching_f_hay=0
real :: sum_NfixT_f=0, sum_Nleaching_f=0

real :: Shade_f

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
DAYS_PRUNT   = CALENDAR_PRUNT(:,:,1:2)
DAYS_THINT   = CALENDAR_THINT(:,:,1:2)
FRPRUNT      = CALENDAR_PRUNT(:,:,3)
FRTHINT      = CALENDAR_THINT(:,:,3)

! INITIAL STATES

! Trees
treedens_t = TREEDENS0
CBT_t      = CBtree0 * treedens_t
CLT_t      = CLtree0 * treedens_t
CPT_t      = 0
CRT_t      = CRtree0 * treedens_t
CST_t      = CStree0 * treedens_t
NLT_t      = CLtree0 * treedens_t * NCLMAXT
LAIT_t     = CLT_t * SLAT

! Agroforestry system
call calcTX
CAtree_t       = KAC * (CBtree0**KACEXP)
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

NfixT_f_hay     = 0.
Nleaching_f_hay = 0.
sum_NfixT_f     = 0.
sum_Nleaching_f = 0.

! Coffee
where (Ac>0.)
  DVS           = DVS0
  CL            = CL0
  CP            = CP0
  CR            = CR0
  CW            = CW0
  NL            = CL * NCLMAX
  LAI           = CL * SLAMAX
endwhere
SENSIT       = 0
SINKP        = 0
SINKPMAXnew  = FSINKPMAX0 * SINKPMAX
harvDM_f_hay = 0
PARold       = 0

do day = 1, NDAYS

! Environment
  call set_weather_day(day, year,doy)
  T_c  = T
  GR_c = GR
  
! Land-cover areas
  call CalcShade(h_t, &
                 LAI,CR,CW,CP,CL,NL,WA,CLITT,CSOMF,CSOMS,NLITT,NSOMF,NSOMS,NMIN, &
                 Ac,f3up)
  call CalcAtc(Ac,TX, At,Atc)
  call RescaleExt_t_tcc(LAIT_t,At,Atc, LAIT_tc,LAIT_c)
  
! Trees
  call morphology(f3up,CBT_t,CST_t,LAIT_t,LAIT_tc,treedens_t, &
                  SAT_t,h_t,hC_t,z,dz,LAIT_tcz)

! Management
  call fert_prun_thin(year,doy,Shade_f,                      &
                      DAYS_FERT ,NFERTV ,DAYS_PRUNC,FRPRUNC, &
                      DAYS_PRUNT,FRPRUNT,DAYS_THINT,FRTHINT)
  treedens_t = treedens_t - thintreedens_t
  
! Trees continued
  RainintT_c = min( RAIN, KRAININTT*LAIT_c )
  call PET(LAIT_c,RainintT_c,T_c,GR_c, PevapT_c,PtranT_c)
  call water_flux(PevapT_c,PtranT_c,TRANCOT,WA, &
                  DUMMY_c,fTranT_c,DUMMY_c,TranT_c)
  call RescaleInt_c_t( fTranT_c,At,Atc, fTranT_t )
  call PARintT(Atc,LAIT_tc,LAIT_tcz, &
               PARintT_c,PARintT_t,PAR_cz,PARintT_tcz)
  call NPP(fTranT_t,PARintT_t)
  
  call Nsupplytree(At,Atc,CRT_t,NMIN, NsupT_t)
  call allocation(day,At,CLT_t,fTranT_t,LAIT_t,NLT_t)
  call NdemandOrgans
  call gtreeNupt(NsupT_t)
  call CNtree(CBT_t,CLT_t,CPT_t,CRT_t,CST_t,fTranT_t,NLT_t)

! Environment continued
  call DDAYL(doy)
  
! Coffee
  PARCOFFEE      = PAR - PARintT_c
  Shade_f        = sum( Ac * (1-PARCOFFEE/PAR) )
  TCOFFEE        = T   - TDIFFMAX * (1. - PARCOFFEE/PAR)
  Rainint        = min( RAIN-RainintT_c, KRAININT*LAI )                     
  PARMA          = sum( PARold,2 ) / 30.
  PARold(:,2:30) = PARold(:,1:29)
  PARold(:,1   ) = PARCOFFEE
  call PET(LAI,Rainint,TCOFFEE,2*PARCOFFEE, Pevap,Ptran)
  call water_flux(Pevap,Ptran,TRANCO,WA, Evap,fTran,RWA,Tran)
  call Nsupply(CR,NMIN,Nsup)
  call abovegroundres(LAI,PARCOFFEE,PARav,PARint)
  
  ! Making the crop sensitive to rainfall (capable of flowering) in the
  ! beginning of the year, and resetting to zero when flowering occurs.
  if ((doy==1).and.(day>TBEFOREP)) SENSIT = 1
  if (DayFl==1)                    SENSIT = 0

  call Phenology(doy,DVS,SENSIT,TCOFFEE,SINKP)
  call growth(TCOFFEE,PARav,PARint,fTran,SINKP,Nsup,PARMA,DVS,fNgrowth)
  call Foliage(fTran)  
  call Senescence(CR,NL,CL,LAI,fTran)
  call PrunHarv(NL,CL,CW,CP,LAI,DVS)
	
! Soil continued
  call water(WA,Rainint+RainintT_c,Evap,Tran+TranT_c,LAI+LAIT_c,RUNOFF,Drain)
  call CNsoil(RWA,WA,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS) 

! STATE EQUATIONS

! Coffee (arrays: [m-2 sun, m-2 shade])
  where(DVS<1.)
    SINKP = SINKP + gSINKP
  elsewhere
    SINKP = 0
  endwhere
  ! Sun and shade coffee plants pruned at the same time
  LAI    = LAI    + adjLAI  + gLAI - dLAI - prunLAI
  NL     = NL     + adjNL   + gNL  - dNL  - prunNL
  CL     = CL     + adjCL   + gCL  - dCL  - prunCL  
  CW     = CW     + adjCW   + gCW         - prunCW
  CR     = CR     + adjCR   + gCR  - dCR
  CP     = CP     + adjCP   + gCP         - harvCP
!  SENSIT = SENSIT + dSENSIT
  DVS    = DVS    + dDVS    - rDVS

! Trees (arrays: m-2 field)
  where (At>0.)
    LAIT_t = CLT_t * SLAT
    CBT_t  = CBT_t + gCBT_t - dCBT_t
    CLT_t  = CLT_t + gCLT_t - dCLT_t
    CPT_t  = CPT_t + gCPT_t - dCPT_t
    CRT_t  = CRT_t + gCRT_t - dCRT_t
    CST_t  = CST_t + gCST_t - dCST_t
    NLT_t  = NLT_t + gNLT_t - dNLT_t
  endwhere
  
! Splitting tree field-fluxes over different land cover classes
  call RescaleExt_t_tcc(dCLT_t    ,At,Atc, DUMMY_tc,dCLTlitt_c)
  call RescaleExt_t_tcc(dCBTlitt_t,At,Atc, DUMMY_tc,dCBTlitt_c)
  call RescaleExt_t_tcc(sCSTsen_t ,At,Atc, DUMMY_tc,sCSTsen_c )
  call RescaleExt_t_tcc(dCRT_t    ,At,Atc, DUMMY_tc,dCRT_c    )
  call RescaleExt_t_tcc(dNLT_t    ,At,Atc, DUMMY_tc,dNLTlitt_c)
  call RescaleExt_t_tcc(dNBTlitt_t,At,Atc, DUMMY_tc,dNBTlitt_c)
  call RescaleExt_t_tcc(dNSTlitt_t,At,Atc, DUMMY_tc,dNSTlitt_c)
  call RescaleExt_t_tcc(dNRsomf_t ,At,Atc, DUMMY_tc,dNRsomf_c )
  call RescaleExt_t_tcc(NfixT_t   ,At,Atc, DUMMY_tc,NfixT_c   )
  call RescaleExt_t_tcc(NuptT_t   ,At,Atc, DUMMY_tc,NuptT_c   )

! Soil (arrays: [m-2 sun, m-2 shade])
  WA    = WA    + adjWA    + RAIN       - Rainint    - RainintT_c - Runoff &
                - Drain    - Evap       - Tran       - TranT_c
  CLITT = CLITT + adjCLITT + dCL        + dCLTlitt_c + dCBTlitt_c + sCSTsen_c &
                - rCLITT   - dCLITT     + prunCL     + prunCW
  CSOMF = CSOMF + adjCSOMF + dCLITTsomf + dCR        + dCRT_c - rCSOMF - dCSOMF
  CSOMS = CSOMS + adjCSOMS + dCSOMFsoms - dCSOMS
  NLITT = NLITT + adjNLITT + dNLTlitt_c + dNBTlitt_c + dNL   + prunNL &
                + prunCW*NCW - rNLITT   - dNLITT     + dNSTlitt_c
  NSOMF = NSOMF + adjNSOMF + dCR*NCR    + dNRsomf_c  + NLITTsomf - rNSOMF - dNSOMF
  NSOMS = NSOMS + adjNSOMS + NSOMFsoms  - dNSOMS
  NMIN  = NMIN  + adjNMIN  + Nfert      + Nmineralisation &
                + NfixT_c  - Nupt       - NuptT_c    - Nleaching - Nemission

! Additional output variables

  Cabg_f      = sum(Ac*(CL + CW + CP))                ! kgC  m-2 field
  CabgT_f     = sum(CLT_t + CST_t + CBT_t + CPT_t)    ! kgC  m-2 field
  Csoil       = CLITT + CSOMF + CSOMS                 ! kgC  m-2 c
  Csoil_f     = sum(Ac*Csoil)                         ! kgC  m-2 field
  C           = Cabg_f  + sum(Ac*CR)                  ! kgC  m-2 field
  CT          = CabgT_f + sum(CRT_t)                  ! kgC  m-2 field
  C_f         = sum(Ac*C )                            ! kgC  m-2 field
  CT_f        = sum(Ac*CT)                            ! kgC  m-2 field
  
  harvDM_f_ha = sum(Ac*harvCP) * 1E4/CCONC            ! kgDM ha-1 field
  if (doy==182) then
  	harvDM_f_hay = 0
  else
	  harvDM_f_hay = harvDM_f_hay + harvDM_f_ha ! kgDM m-2 field y-1
  endif

  LAI_f       = sum(Ac*LAI)                   ! m2   m-2 field
  LAIT        = sum(LAIT_t)/sum(At)           ! m2   m-2 shade
  Nsoil       = NLITT + NSOMF + NSOMS + NMIN  ! kgN  m-2 c
  Nsoil_f     = sum(Ac*Nsoil)                 ! kgN  m-2 field
  WA_f        = sum(Ac*WA)                    ! kgW  m-2 field
  WC_f        = 0.001 * WA_f / ROOTD   	      ! m3W  m-3
  
! N-balance soil (kgN m-2 field d-1): Change in NLITT+NSOMF+NSOMS+NMIN
  Nfert_f     = Nfert                         ! Fertilisation
  NfixT_f     = sum(Ac*NfixT_c)               ! N-fixation trees
  NsenprunT_f = sum(Ac*(dNLTlitt_c + dNBTlitt_c + dNSTlitt_c + dNRsomf_c))
                                              ! Senescence + pruning trees
  Nsenprun_f  = sum(Ac*(dNL + prunNL + prunCW*NCW + dCR*NCR))
                                              ! Senescence + pruning coffee
  Nleaching_f = sum(Ac*Nleaching)             ! Leaching from soil
  Nemission_f = sum(Ac*Nemission)             ! Emission from soil
  Nrunoff_f   = sum(Ac*(rNLITT + rNSOMF))     ! Runoff litter + SOMF
  Nupt_f      = sum(Ac*Nupt)                  ! N-uptake coffee
  NuptT_f     = sum(Ac*NuptT_c)               ! N-uptake trees
  
! C-balance soil (kgC m-2 field d-1): Change in CLITT+CSOMF+CSOMS
  CsenprunT_f = sum(Ac*(dCLTlitt_c + dCBTlitt_c + sCSTsen_c + dCRT_c))
                                              ! Senescence + pruning trees
  Csenprun_f  = sum(Ac*(dCL + prunCL + prunCW + dCR))
                                              ! Senescence + pruning coffee
  Rsoil_f     = sum(Ac*Rsoil)                 ! Soil respiration
  Crunoff_f   = sum(Ac*(rCLITT + rCSOMF))     ! Runoff litter + SOMF
  
! H2O-balance soil (kgW m-2 field d-1 = mm d-1): Change in WA
  Rain_f      = RAIN
  Drain_f     = sum(Ac*Drain)
  Runoff_f    = sum(Ac*Runoff)
  Evap_f      = sum(Ac*Evap)
  Tran_f      = sum(Ac*Tran)
  TranT_f     = sum(Ac*TranT_c)
  Rainint_f   = sum(Ac*Rainint)
  RainintT_f  = sum(Ac*RainintT_c)
  
! C-balance coffee (kgC m-2 field d-1)
  gC_f        = sum(Ac* (gCL + gCW + gCR + gCP)) ! Growth coffee
  dC_f        = sum(Ac* (dCL + dCR))             ! Senescence coffee
  prunC_f     = sum(Ac* (prunCL + prunCW))       ! Pruning coffee
  harvCP_f    = sum(Ac*harvCP)                   ! Harvesting coffee
  
! C-balance system (kgC m-2 field d-1):
! Change in CL+CW+CR+CP + CLT_t+CST_t+CBT_t+CPT_t+CRT_t + CLITT+CSOMF+CSOMS
  gCT_f       = sum(gCLT_t + gCST_t + gCBT_t + gCPT_t + gCRT_t)
  harvCBT_f   = sum(harvCBT_t)
  harvCPT_f   = sum(harvCPT_t)
  harvCST_f   = sum(harvCST_t)
  ! gC_f, Crunoff_f, harvCP_f, Rsoil_f
  
! C-distribution within coffee (kgC m-2 field):
  CR_f        = sum(Ac*CR)
  CW_f        = sum(Ac*CW)
  CL_f        = sum(Ac*CL)
  CP_f        = sum(Ac*CP)

! Average process rates over the simulated period
  sum_NfixT_f     = sum_NfixT_f     + NfixT_f
  NfixT_f_hay     = sum_NfixT_f * 1E4 * 365 / day
  sum_Nleaching_f = sum_Nleaching_f + Nleaching_f
  Nleaching_f_hay = sum_Nleaching_f * 1E4 * 365 / day

! Average C- and N-balances over the simulated period 
  if(day==1) then
    Nsoil_f1      = Nsoil_f
    Csoil_f1      = Csoil_f 
    Csys_f1       = Csoil_f + C_f + CT_f
  else
    D_Nsoil_f_hay = (Nsoil_f          - Nsoil_f1) * 1E4 * 365 / (day-1)
    D_Csoil_f_hay = (Csoil_f          - Csoil_f1) * 1E4 * 365 / (day-1)
    D_Csys_f_hay  = (Csoil_f+C_f+CT_f - Csys_f1 ) * 1E4 * 365 / (day-1)
  endif

! Outputs
! The "c1", "c2" etc. in the units below refer to parts of the field with
! specific combinations of shade tree species:
! c1 = sun (no tree species at all)
! c2 = shaded by tree sp. 1     (lower-stratum tree sp.)
! c3 = shaded by tree sp. 2     (lower-stratum tree sp.)
! c4 = shaded by tree sp. 3     (upper-stratum tree sp.)
! c5 = shaded by tree sp. 1 & 3 (lower- and upper-stratum tree spp.)
! c6 = shaded by tree sp. 2 & 3 (lower- and upper-stratum tree spp.)
  y(day,  1    ) = year + (doy-0.5)/366 ! "Time" = Decimal year (approx.)
  y(day,  2    ) = year
  y(day,  3    ) = doy
  y(day,  4:  9) = Ac                 ! m2 c m-2
  y(day, 10: 12) = At                 ! m2 t m-2
  y(day, 13: 18) = fNgrowth           ! -
  y(day, 19: 24) = fTran              ! -
  y(day, 25    ) = Cabg_f  		        ! kgC  m-2
  y(day, 26: 31) = harvCP             ! kgC  m-2 c
  y(day, 32    ) = harvDM_f_hay       ! kgDM ha-1 y-1
  y(day, 33: 38) = LAI                ! m2   m-2 c
  y(day, 39    ) = CabgT_f 		        ! kgC  m-2
  y(day, 40: 42) = CAtree_t           ! m2   tree-1
  y(day, 43: 45) = h_t                ! m
  y(day, 46: 51) = LAIT_c             ! m2   m-2 c
  y(day, 52: 54) = treedens_t         ! m-2
  y(day, 55    ) = Csoil_f            ! kgC  m-2
  y(day, 56    ) = Nsoil_f            ! kgN  m-2
  y(day, 57: 59) = CST_t              ! kgC  m-2
  y(day, 60: 62) = SAT_t              ! m2   m-2
  
  y(day, 63    ) = Nfert_f            ! kgN  m-2 d-1
  y(day, 64    ) = NfixT_f            ! kgN  m-2 d-1
  y(day, 65    ) = NsenprunT_f        ! kgN  m-2 d-1
  y(day, 66    ) = Nsenprun_f         ! kgN  m-2 d-1
  y(day, 67    ) = Nleaching_f        ! kgN  m-2 d-1
  y(day, 68    ) = Nemission_f        ! kgN  m-2 d-1
  y(day, 69    ) = Nrunoff_f          ! kgN  m-2 d-1
  y(day, 70    ) = Nupt_f             ! kgN  m-2 d-1
  y(day, 71    ) = NuptT_f            ! kgN  m-2 d-1
  
  y(day, 72: 77) = CLITT              ! kgC  m-2 c
  y(day, 78: 83) = NLITT              ! kgN  m-2 c
  y(day, 84: 86) = harvCST_t          ! kgC  m-2 d-1
  y(day, 87: 89) = harvNST_t          ! kgN  m-2 d-1
  
  y(day, 90    ) = CsenprunT_f        ! kgC  m-2 d-1
  y(day, 91    ) = Csenprun_f         ! kgC  m-2 d-1
  y(day, 92    ) = Rsoil_f            ! kgC  m-2 d-1
  y(day, 93    ) = Crunoff_f          ! kgC  m-2 d-1
  
  y(day, 94    ) = WA_f               ! mm
  y(day, 95    ) = Rain_f             ! mm   d-1
  y(day, 96    ) = Drain_f            ! mm   d-1
  y(day, 97    ) = Runoff_f           ! mm   d-1
  y(day, 98    ) = Evap_f             ! mm   d-1
  y(day, 99    ) = Tran_f             ! mm   d-1
  y(day,100    ) = TranT_f            ! mm   d-1
  y(day,101    ) = Rainint_f          ! mm   d-1
  y(day,102    ) = RainintT_f         ! mm   d-1
  
  y(day,103    ) = C_f                ! kgC  m-2
  y(day,104    ) = gC_f               ! kgC  m-2 d-1
  y(day,105    ) = dC_f               ! kgC  m-2 d-1
  y(day,106    ) = prunC_f            ! kgC  m-2 d-1
  y(day,107    ) = harvCP_f           ! kgC  m-2 d-1
  
  y(day,108    ) = CT_f               ! kgC  m-2
  y(day,109    ) = gCT_f              ! kgC  m-2 d-1
  y(day,110    ) = harvCBT_f          ! kgC  m-2 d-1
  y(day,111    ) = harvCPT_f          ! kgC  m-2 d-1
  y(day,112    ) = harvCST_f          ! kgC  m-2 d-1
  
  y(day,113:115) = CPT_t              ! kgC  m-2
  y(day,116:118) = harvCPT_t          ! kgC  m-2 d-1
  y(day,119:121) = harvNPT_t          ! kgN  m-2 d-1
  
  y(day,122    ) = DayFl              ! -
  y(day,123    ) = DVS(1)             ! -
  y(day,124    ) = SINKP(1)           ! -
  y(day,125    ) = SINKPMAXnew(1)     ! -
  y(day,126    ) = PARMA(1)           ! MJ   m-2 d-1
  y(day,127    ) = DVS(2)             ! -
  y(day,128    ) = SINKP(2)           ! -
  y(day,129    ) = SINKPMAXnew(2)     ! -
  y(day,130    ) = PARMA(2)           ! MJ   m-2 d-1

  y(day,131    ) = CR_f               ! kgC  m-2
  y(day,132    ) = CW_f               ! kgC  m-2
  y(day,133    ) = CL_f               ! kgC  m-2
  y(day,134    ) = CP_f               ! kgC  m-2

  y(day,135:137) = CRT_t              ! kgC  m-2
  y(day,138:140) = CBT_t              ! kgC  m-2
  y(day,141:143) = CLT_t              ! kgC  m-2

  y(day,144:146) = LAIT_t             ! m2 t m-2
  y(day,147:149) = fTranT_t           ! -
  
  y(day,150    ) = D_Csoil_f_hay      ! kgC  ha-1 y-1
  y(day,151    ) = D_Csys_f_hay       ! kgC  ha-1 y-1
  y(day,152    ) = D_Nsoil_f_hay      ! kgN  ha-1 y-1
  
  y(day,153    ) = NfixT_f_hay        ! kgN  ha-1 y-1
  y(day,154    ) = Nleaching_f_hay    ! kgN  ha-1 y-1
  
  y(day,155    ) = Shade_f            ! -
  
  y(day,156:161) = z                  ! m

  y(day,162    ) = f3up               ! -
  
  y(day,163:168) = DayHarv            ! -

! CALIBRATION VARIABLES IN CAF2021's AND ORIANA's ORIGINAL BC DATA FILES.
! ------------------------------------------------------------------------
! NAME in CAF2021 ! NAME in original data files   ! UNIT
! (this version)  ! (if different)                !
! ------------------------------------------------------------------------
! 1.-Ac(1)        ! SA                            ! m2 shade m-2  field
! Cabg_f          ! CT                            ! kg C     m-2  field
! CabgT_f         ! CTT                           ! kg C     m-2  field
! CAtree_t(1)     ! CAtree                        ! m2       tree-1
! CL(1)           !                               ! kg C     m-2  c1
! CL(2)           !                               ! kg C     m-2  c2
! Csoil_f         ! Csoilave      (t C ha-1)      ! kg C     m-2  field
! CW(1)           !                               ! kg C     m-2  c1
! CW(2)           !                               ! kg C     m-2  c2
! h_t(1)          ! h                             ! m
! harvCP(1)       !                               ! kg C     m-2  c1
! harvCP(2)       !                               ! kg C     m-2  c2
! harvDM_f_hay    ! harvDMav_year (t DM ha-1 y-1) ! kg DM    ha-1 field y-1
! LAI_f           ! LAIave                        ! m2       m-2  field
! LAI(1)          !                               ! m2       m-2  c1
! LAI(2)          !                               ! m2       m-2  c2
! LAIT            !                               ! m2       m-2  shade
! Nsoil_f         ! Nsoilave      (t N ha-1)      ! kg N     m-2  field
! WC_f            ! WC_F                          ! m3 W     m-3  field

!if(day==NDAYS) then
!  write(66,*) "---------------------------------------------------"
!  write(66,"('day=N, Ac='          , 6F8.4)") Ac
!  write(66,"('day=N, At='          , 6F8.4)") At
!  write(66,"('day=N, Atc='         , 6F8.4)") Atc
!  write(66,"('day=N, h_t='         , 6F8.4)") h_t
!  write(66,"('day=N, LAIT_c='      , 6F8.4)") LAIT_c
!  write(66,"('day=N, LAIT_t='      , 6F8.4)") LAIT_t
!  write(66,"('day=N, LAIT_tc='     , 6F8.4)") LAIT_tc
!  write(66,"('day=N, LAIT_tcz='    , 6F8.4)") LAIT_tcz
!  write(66,"('day=N, PAR_cz='      , 6F8.4)") PAR_cz
!  write(66,"('day=N, PARintT_c='   , 6F8.4)") PARintT_c
!  write(66,"('day=N, PARintT_t='   , 6F8.4)") PARintT_t
!  write(66,"('day=N, PARintT_tcz=' , 6F8.4)") PARintT_tcz
!  write(66,*) "day=N, x=", x
!endif

enddo ! end time loop

!close(66)

end  
