Module tree

! Subroutines: morphology, PARintT, NPP, allocation, NdemandOrgans, gtreeNupt,
!              CNtree

use declare_parameters
use environment
use management
implicit none

! Morphology
real :: CAtree_t(nt)=0, CBpertree_t(nt)=0, CSpertree_t(nt)=0, SAT_t(nt)
real :: h_t     (nt)=0

! NPP
real :: fLUEco2, fLUEt
real :: GPP_t    (nt)=0, NPPmaxN_t(nt)=0
real :: PARintT_c(nc)=0, PARintT_t(nt)=0

! Allocation
real :: fGILAI_t(nt)=0, fGIN_t(nt)=0, FLT_t(nt)=0
real :: FST_t(nt)=0   , FBT_t(nt)=0 , FRT_t(nt)=0, FPT_t(nt)=0

! NdemandOrgans 
real :: gCBmaxN_t(nt), gCLmaxN_t(nt)    , gCPmaxN_t(nt), gCRmaxN_t(nt), gCSmaxN_t(nt)
real :: NBdemT_t (nt), NLdemTmax_t(nt)=0, NPdemT_t (nt), NRdemT_t (nt), NSdemT_t (nt)

! gtreeNupt
real :: fNgrowth_t(nt)=0
real :: gCBT_t   (nt)=0, gCLT_t (nt)=0
real :: gCRT_t   (nt)=0, gCST_t (nt)=0, gCPT_t   (nt)=0
real :: gNBT_t   (nt)=0, gNLT_t (nt)=0, gNLTmax_t(nt)=0
real :: gNPT_t   (nt)=0, gNRT_t (nt)=0, gNST_t   (nt)=0
real :: NCLTnew_t(nt)=0, NdemT_t(nt)=0, NuptT_t  (nt)=0
real :: NfixT_t  (nt)=0

! CNtree
real :: dCBT_t   (nt)=0, dCLT_t(nt)=0, dCPT_t(nt)=0, dCRT_t(nt)=0, dCST_t(nt)=0
real :: dNBlitt_t(nt)=0, dNSlitt_t(nt)=0, dNLT_t(nt)=0, dNPT_t(nt)=0, dNRsomf_t(nt)=0
real :: harvCPT_t(nt)=0, harvNPT_t(nt)=0
real :: harvCST_t(nt)=0, harvNST_t(nt)=0
real :: NCLT_t   (nt)
real :: sCBTman_t(nt)=0, sCLTman_t(nt)=0, sCRTman_t(nt)=0, sCSTman_t(nt)=0
real :: sCBTsen_t(nt)=0, sCLTsen_t(nt)=0, sCRTsen_t(nt)=0, sCSTsen_t(nt)=0

Contains

  Subroutine morphology(CBT_t,CST_t,LAIT_t,treedens_t, SAT_t)
  real    :: CBT_t(nt),CST_t(nt),LAIT_t(nt),treedens_t(nt)
  real    :: SAT_t(nt)
  where (treedens_t>0.)
    CSpertree_t = CST_t / treedens_t
    CBpertree_t = CBT_t / treedens_t
  elsewhere
    CSpertree_t = 0.
    CBpertree_t = 0.
  endwhere
  h_t      = KH  * (CSpertree_t**KHEXP )
  CAtree_t = KAC * (CBpertree_t**KACEXP)
  SAT_t    = CAtree_t * treedens_t * SHADEPROJ
  SAT_t(1:ntlow) = SAT_t(1:ntlow) / max(1., sum(SAT_t(1:ntlow))) ! sum(SAT_t(1:2)) <= 1
  SAT_t(3)       = min(1., SAT_t(3))                             ! SAT_t(3)) <= 1
  end Subroutine morphology  

  Subroutine PARintT(Ac,Atc,LAIT_tc, PARintT_c,PARintT_t)
  real    :: Ac(nc), Atc(nt,nc), LAIT_tc(nt,nc)
  real    :: PARintT_c(nc), PARintT_t(nt)
  real    :: PARbelowT3_c(nc), PARintT_tc(nt,nc)
  integer :: it
  PARintT_tc      = 0
  PARintT_tc(3,:) = PAR * (1. - exp(-KEXTT*LAIT_tc(3,:)))
  PARbelowT3_c    = PAR-PARintT_tc(3,:)
  do it=1,ntlow
    PARintT_tc(it,:) = PARbelowT3_c * &
	                   (1. - exp(-KEXTT*LAIT_tc(it,:)))
  enddo
  PARintT_c = sum( PARintT_tc, dim=1 )
  do it=1,nt
    PARintT_t(it) = sum( Atc(it,:) * PARintT_tc(it,:) )
  enddo  
  end Subroutine PARintT

  Subroutine NPP(fTranT_t,PARintT_t)
  real :: fTranT_t(nt), PARintT_t(nt)
  real :: LUEtree_t(nt)
  fLUEco2   = 1 + beta*log(co2a/co20)
  fLUEt     = exp( -0.5*((T - toptt)/ttolt)**2. )
  LUEtree_t = fLUEco2 * fLUET * LUEMAX * fTranT_t
  GPP_t     = PARintT_t * LUEtree_t
  NPPmaxN_t = GPP_t * (1-GAMMA)
  end Subroutine NPP

  Subroutine allocation(day,At,CLT_t,fTranT_t,LAIT_t,NLT_t)
  integer :: day
  real    :: At(nt),CLT_t(nt),fTranT_t(nt),LAIT_t(nt),NLT_t(nt)
  real    :: FWT_t(nt),FPRT_t(nt)
  where (At>0) fGILAI_t = max(0., 1. - (LAIT_t/At) / LAIMAXT )  
  where (CLT_t>0.)
    NCLT_t = NLT_t / CLT_t
  elsewhere
    NCLT_t = 0.     
  endwhere
  fGIN_t   = max(0.,min(1., (NCLT_t/NCLmaxT - fNCLminT)/(1 - fNCLminT) ))
  FLT_t    = FLTmax * fGILAI_t * fGIN_t * fTranT_t
  FWT_t    = (1. - FLT_t) * FWT
    FST_t  = FWT_t  * FST
    FBT_t  = FWT_t  - FST_t
  FPRT_t   = 1. - FLT_t - FWT_t
    FPT_t  = 0
    if (day>TBEFOREPT) FPT_t = FPRT_t * FPT
    FRT_t  = FPRT_t - FPT_t
  end Subroutine allocation

  Subroutine NdemandOrgans
  gCLmaxN_t   = FLT_t   * NPPmaxN_t
  gCSmaxN_t   = FST_t   * NPPmaxN_t
  gCBmaxN_t   = FBT_t   * NPPmaxN_t
  gCPmaxN_t   = FPT_t   * NPPmaxN_t
  gCRmaxN_t   = FRT_t   * NPPmaxN_t
  NLdemTmax_t = NCLmaxT * gCLmaxN_t
  NSdemT_t    = NCWT    * gCSmaxN_t
  NBdemT_t    = NCWT    * gCBmaxN_t
  NPdemT_t    = NCPT    * gCPmaxN_t
  NRdemT_t    = NCRT    * gCRmaxN_t
  end Subroutine NdemandOrgans

  Subroutine gtreeNupt(NsupT_t)
  real NsupT_t(nt)
  NdemT_t = NLdemTmax_t + NSdemT_t + NBdemT_t + NPdemT_t + NRdemT_t
  NuptT_t = min( NsupT_t, NdemT_t )
  where (NdemT_t>0.)
    fNgrowth_t = max(0.,min(1., NuptT_t/NdemT_t ))
  elsewhere
    fNgrowth_t = 1.
  endwhere
  gNLTmax_t = fNgrowth_t * NLdemTmax_t
  gNLT_t    = fNgrowth_t * gNLTmax_t
  gNST_t    = fNgrowth_t * NSdemT_t
  gNBT_t    = fNgrowth_t * NBdemT_t
  gNPT_t    = fNgrowth_t * NPdemT_t
  gNRT_t    = fNgrowth_t * NRdemT_t
  NCLTnew_t = ( fNCLminT + fNgrowth_t*(1.-fNCLminT) ) * NCLmaxT 
  gCLT_t    = min( gNLT_t/NCLTnew_t, FLT_t*NPPmaxN_t )
  gCST_t    =  gNST_t / NCWT
  gCBT_t    =  gNBT_t / NCWT
  gCPT_t    =  gNPT_t / NCPT
  gCRT_t    = (gNRT_t + (gNLTmax_t - gNLT_t)) / NCRT
  NfixT_t   =  gCRT_t * KNFIX
  end Subroutine gtreeNupt

  Subroutine CNtree(CBT_t,CLT_t,CPT_t,CRT_t,CST_t,fTranT_t,NLT_t)
  real :: CBT_t(nt), CLT_t(nt), CPT_t(nt), CRT_t(nt), CST_t(nt)
  real :: fTranT_t(nt), NLT_t(nt)
! Leaves
  sCLTman_t    = (thinFRT + prunFRT) * CLT_t
  sCLTsen_t    = ( CLT_t / (fTranT_t+FTCLminT*(1-fTranT_t)) ) / TCLmaxT
  dCLT_t       = sCLTman_t + sCLTsen_t
  dNLT_t       = dCLT_t * NCLT_t
! Stems 
  sCSTman_t    = thinFRT * CST_t
  sCSTsen_t    = CST_t / TCST
  dCST_t       = sCSTman_t + sCSTsen_t
  harvCST_t    = sCSTman_t
  harvNST_t    = sCSTman_t * NCWT
  dNSlitt_t    = sCSTsen_t * NCWT
! Branches
  sCBTman_t    = (thinFRT + prunFRT) * CBT_t
  sCBTsen_t    = CBT_t / TCBT
  dCBT_t       = sCBTman_t + sCBTsen_t
  dNBlitt_t    = dCBT_t * NCWT
! Products (fruit)
  dCPT_t       = CPT_t / TCPTHARV
  harvCPT_t    = dCPT_t
  harvNPT_t    = dCPT_t * NCPT
! Roots  
  sCRTman_t    = thinFRT * CRT_t
  sCRTsen_t    = CRT_t / TCRT
  dCRT_t       = sCRTman_t + sCRTsen_t
  dNRsomf_t    = dCRT_t * NCRT
  end Subroutine CNtree
  
end Module tree 

