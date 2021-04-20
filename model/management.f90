module management

! Subroutines: fert_prune_thin

use declare_parameters
use environment
implicit none
integer :: PRUN, NOPRUN
real    :: Nfert, prunFRC, prunFRT(nt), thinFRT(nt)
real    :: treedens_t(nt), thintreedens_t(nt)

Contains     

  Subroutine fert_prune_thin(year,doy,DAYS_FERT ,NFERTV ,DAYS_PRUNC,FRPRUNC, &
                                      DAYS_PRUNT,FRPRUNT,DAYS_THINT,FRTHINT)
  integer                  :: year,doy,i,t
  integer,dimension(  100,2) :: DAYS_FERT , DAYS_PRUNC
  real   ,dimension(  100  ) ::             FRPRUNC
  integer,dimension(3,100,2) :: DAYS_PRUNT, DAYS_THINT
  real   ,dimension(3,100  ) :: FRPRUNT   , FRTHINT
  integer,dimension(  100  ) :: NFERTV
    Nfert   = 0
    PRUN    = 0
    NOPRUN  = 1
    prunFRT = 0
    thinFRT = 0
    do i=1,100    
      if ( (year==DAYS_FERT (i,1)) .and. (doy==DAYS_FERT (i,2)) ) then
        Nfert = NFERTMULT*NFERTV(i)/10000.
	    end if
      if ( (year==DAYS_PRUNC(i,1)) .and. (doy==DAYS_PRUNC(i,2)) ) then
        PRUN    = 1
        NOPRUN  = 0
        prunFRC = FRPRUNC(i)
  	  end if
	    do t=1,nt
        if ( (year==DAYS_PRUNT(t,i,1)) .and. (doy==DAYS_PRUNT(t,i,2)) ) then
          prunFRT(t) = FRPRUNT(t,i)
	      end if
        if ( (year==DAYS_THINT(t,i,1)) .and. (doy==DAYS_THINT(t,i,2)) ) then
          thinFRT(t) = FRTHINT(t,i)
	      end if
	    end do
    end do
    thintreedens_t = treedens_t * thinFRT
  end Subroutine fert_prune_thin
    
end module management      
