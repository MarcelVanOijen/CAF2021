module management

! Subroutines: fert_prun_thin

use declare_parameters
use environment
implicit none
real :: Nfert, prunFRC, prunFRT(nt), thinFRT(nt)
real :: treedens_t(nt), thintreedens_t(nt)

Contains     

  Subroutine fert_prun_thin(year,doy,Shade_f, &
    DAYS_FERT ,NFERTV ,DAYS_PRUNC,FRPRUNC, &
    DAYS_PRUNT,FRPRUNT,DAYS_THINT,FRTHINT )
  integer                    :: year,doy,i,t
  real                       :: Shade_f
  integer,dimension(  100,2) :: DAYS_FERT , DAYS_PRUNC
  real   ,dimension(  100  ) ::             FRPRUNC
  integer,dimension(3,100,2) :: DAYS_PRUNT, DAYS_THINT
  real   ,dimension(3,100  ) :: FRPRUNT   , FRTHINT
  integer,dimension(  100  ) :: NFERTV
    Nfert   = 0
    prunFRC = 0
    prunFRT = 0
    thinFRT = 0
    do i=1,100    
      if ( (year==DAYS_FERT (i,1)) .and. (doy==DAYS_FERT (i,2)) ) then
        Nfert = NFERTMULT*NFERTV(i)/10000.
	    endif
      if ( (year==DAYS_PRUNC(i,1)) .and. (doy==DAYS_PRUNC(i,2)) ) then
        prunFRC = FRPRUNC(i)
  	  endif
	    do t=1,nt
        if ( (year==DAYS_PRUNT(t,i,1)) .and. (doy==DAYS_PRUNT(t,i,2)) ) then
          prunFRT(t) = FRPRUNT(t,i)
          if ((SHADETARGET>0).and.(Shade_f>0)) then
            prunFRT(t) = 1 - SHADETARGET / Shade_f
          endif
	      endif
        if ( (year==DAYS_THINT(t,i,1)) .and. (doy==DAYS_THINT(t,i,2)) ) then
          thinFRT(t) = FRTHINT(t,i)
	      endif
	    enddo
    enddo
    thintreedens_t = treedens_t * thinFRT
    
    
  end Subroutine fert_prun_thin
    
end module management      
