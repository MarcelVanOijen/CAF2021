Module tree

! Subroutines: morphology, PARintT, NPP, allocation, NdemandOrgans, gtreeNupt,
!              CNtree

use declare_parameters
use environment
use management
implicit none

! Morphology
real :: CAtree_t(nt)=0, CBpertree_t(nt)=0, CSpertree_t(nt)=0
real :: SAT_t(nt)=0, h_t(nt)=0, hC_t(nt)=0, z(nz)=0
real :: dz(nz)=0, LAIT_tcz(nt,nc,nz)=0

! PARintT & NPP
real :: fLUEco2, fLUEt
real :: GPP_t    (nt)=0   , NPPmaxN_t(nt)=0
real :: PARintT_c(nc)=0   , PARintT_t(nt)=0
real :: PAR_cz(nc,nz)=0   , PARintT_tcz(nt,nc,nz)=0

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
real :: dCBlitt_t(nt)=0, dCBT_t   (nt)=0, dCLT_t   (nt)=0
real :: dCPT_t   (nt)=0, dCRT_t   (nt)=0, dCST_t   (nt)=0
real :: dNBlitt_t(nt)=0, dNSlitt_t(nt)=0, dNLT_t   (nt)=0
real :: dNPT_t   (nt)=0, dNRsomf_t(nt)=0
real :: harvCBT_t(nt)=0, harvCPT_t(nt)=0, harvCST_t(nt)=0
real :: harvNBT_t(nt)=0, harvNPT_t(nt)=0, harvNST_t(nt)=0
real :: NCLT_t   (nt)
real :: sCBTman_t(nt)=0, sCLTman_t(nt)=0, sCRTman_t(nt)=0, sCSTman_t(nt)=0
real :: sCBTsen_t(nt)=0, sCLTsen_t(nt)=0, sCRTsen_t(nt)=0, sCSTsen_t(nt)=0

Contains

  Subroutine morphology(f3up, &
    CBT_t,CST_t,LAIT_t,LAIT_tc,treedens_t, &
    SAT_t,h_t,hC_t,z,dz,LAIT_tcz)
  real, intent(in)  :: f3up
  real, intent(in)  :: CBT_t(nt),CST_t(nt),LAIT_t(nt),LAIT_tc(nt,nc),treedens_t(nt)
  real, intent(out) :: SAT_t(nt),h_t(nt),hC_t(nt),z(nz)
  real, intent(out) :: dz(nz), LAIT_tcz(nt,nc,nz)
  real              :: z_unsorted(nz)
  real              :: f3lo
  logical           :: mk(nz)
  integer           :: ic,it,iz
  
  where (treedens_t>0.)
    CSpertree_t = CST_t / treedens_t
    CBpertree_t = CBT_t / treedens_t
  elsewhere
    CSpertree_t = 0.
    CBpertree_t = 0.
  endwhere

  where (CSpertree_t<1.)
    h_t           = min( HMAX, KH *  CSpertree_t          )
  elsewhere
    h_t           = min( HMAX, KH * (CSpertree_t**KHEXP ) )
  endwhere
  hC_t(1)         = h_t(1) * 0.5
  hC_t(2)         = h_t(2) * 0.5
  hC_t(3)         = h_t(3) * 0.5
  z_unsorted(1:3) = h_t
  z_unsorted(4:6) = h_t - hC_t
  mk              = .TRUE.
  do iz=1,(nz-1)
    z(iz) = maxval(z_unsorted,mk)
    mk(maxloc(z_unsorted,mk)) = .FALSE.
  enddo
  z(6) = minval(z_unsorted)
  
  dz(1:(nz-1)) = z(1:(nz-1)) - z(2:nz)
  dz(nz)       = 0
  
  LAIT_tcz = 0
  do ic=2,nc
    do it=1,nt
      do iz=1,(nz-1)
        if( (  dz(iz)            >  0       ) .AND. &
            (  h_t(it)           >= z(iz)   ) .AND. &
            ( (h_t(it)-hC_t(it)) <= z(iz+1) ) ) then
          LAIT_tcz(it,ic,iz) = LAIT_tc(it,ic) * dz(iz) / hC_t(it)
        endif
      enddo
    enddo
  enddo
  
  CAtree_t       = KAC * (CBpertree_t**KACEXP)
  SAT_t          = CAtree_t * treedens_t * SHADEPROJ
  
  f3lo = 1 - f3up
  
  SAT_t(1:ntlow) = f3up * SAT_t(1:ntlow) / max(1., sum(SAT_t(1:ntlow))) + &
                   f3lo * SAT_t(1:ntlow) / max(1., sum(SAT_t))
  SAT_t(3)       = f3up * min(1., SAT_t(3)) + &
                   f3lo * SAT_t(3)       / max(1., sum(SAT_t))
!  SAT_t(1:ntlow) = SAT_t(1:ntlow) / max(1., sum(SAT_t(1:ntlow))) ! sum(SAT_t(1:2)) <= 1
!  SAT_t(3)       = min(1., SAT_t(3))                             ! SAT_t(3)) <= 1

  end Subroutine morphology  

  Subroutine PARintT(Atc,LAIT_tc,LAIT_tcz, &
                     PARintT_c,PARintT_t,PAR_cz,PARintT_tcz)
  real, intent(in)  :: Atc(nt,nc), LAIT_tc(nt,nc), LAIT_tcz(nt,nc,nz)
  real, intent(out) :: PARintT_c(nc), PARintT_t(nt)
  real, intent(out) :: PAR_cz(nc,nz), PARintT_tcz(nt,nc,nz)
!  real              :: PARbelowT3_c(nc)
  real              :: PARintT_tc(nt,nc)
  integer           :: ic,it,iz
!  PARintT_tc      = 0
!  PARintT_tc(3,:) = PAR * (1. - exp(-KEXTT*LAIT_tc(3,:)))
!  PARbelowT3_c    = PAR-PARintT_tc(3,:)
!  do it=1,ntlow
!    PARintT_tc(it,:) = PARbelowT3_c * &
!	                   (1. - exp(-KEXTT*LAIT_tc(it,:)))
!  enddo
!  PARintT_c = sum( PARintT_tc, dim=1 )
!  do it=1,nt
!    PARintT_t(it) = sum( Atc(it,:) * PARintT_tc(it,:) )
!  enddo
  
  PAR_cz      = 0
  PAR_cz(:,1) = PAR
  do ic=1,nc
    do iz=2,nz
      PAR_cz(ic,iz) = PAR_cz(ic,iz-1) * &
                      exp( -sum(KEXTT*LAIT_tcz(:,ic,iz-1)) )
    enddo
  enddo
  
  PARintT_tcz = 0
  PARintT_tc  = 0
  do it=1,nt
    do ic=2,nc
      do iz=1,(nz-1)
        if( LAIT_tcz(it,ic,iz)>0 ) then
          PARintT_tcz(it,ic,iz) = (PAR_cz(ic,iz) - PAR_cz(ic,iz+1)) * &
                                       KEXTT*LAIT_tcz(it,ic,iz) /     &
                                  sum( KEXTT*LAIT_tcz(: ,ic,iz) )
        endif
      enddo
      PARintT_tc(it,ic) = sum( PARintT_tcz(it,ic,:) )
    enddo
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
  FLT_t    = FLTMAX * fGILAI_t * fGIN_t * fTranT_t
  FWT_t    = (1. - FLT_t) * FWT
    FST_t  = FWT_t  * FST
    FBT_t  = FWT_t  - FST_t
  FPRT_t   = 1. - FLT_t - FWT_t
    FPT_t  = 0
    where (day>TBEFOREPT) FPT_t = FPRT_t * FPT
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
  harvCBT_t    = sCBTman_t * FHARVBT
  harvNBT_t    = sCBTman_t * FHARVBT * NCWT
  dCBlitt_t    = dCBT_t - harvCBT_t
  dNBlitt_t    = dCBlitt_t * NCWT
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

