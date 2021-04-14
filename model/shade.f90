Module shade

! Subroutines: CalcShade

use declare_parameters
use tree
implicit none

real :: Achange
real :: adjLAI(nc)  , adjCR(nc)   , adjCW(nc)   , adjCP(nc)   , adjCL(nc), adjNL(nc)
real :: adjWA(nc)   , adjCLITT(nc), adjCSOMF(nc), adjCSOMS(nc)
real :: adjNLITT(nc), adjNSOMF(nc), adjNSOMS(nc), adjNMIN(nc)

Contains

  Subroutine CalcShade(LAI,CR,CW,CP,CL,NL,WA,CLITT,CSOMF,CSOMS,NLITT,NSOMF,NSOMS,NMIN, Ac)
  integer :: i,j
  real    :: AcOld(nc), dAc(nc), dfAfromto(nc,nc)
  real    :: LAI(:), CR(:), CW(:), CP(:), CL(:), NL(:)
  real    :: WA(:), CLITT(:), CSOMF(:), CSOMS(:), NLITT(:), NSOMF(:), NSOMS(:), NMIN(:)
  real    :: Ac(nc)
  AcOld   = Ac
  
  Ac(5:6) = SAT_t(3) * SAT_t(1:2)
  Ac(4)   = max( 0., SAT_t(3) * (1-SAT_t(1)-SAT_t(2)) )
  Ac(2:3) = max( 0., (1.-SAT_t(3)) * SAT_t(1:2) )
  Ac(1)   = max( 0., 1. - sum(Ac(2:nc)) )
	
  dAc       = Ac - AcOld
  Achange   = sum( dAc, MASK=dAc>0. )
  dfAfromto = 0
  do i = 1,nc
     do j = 1,nc
	    if((dAc(i)<0).AND.(dAc(j)>0)) then
		  dfAfromto(i,j) = -dAc(i) * dAc(j) / Achange / Ac(j)
		endif
	 enddo
  enddo
  adjCL    = 0
  adjCP    = 0
  adjCR    = 0
  adjCW    = 0
  adjNL    = 0
  adjLAI   = 0
  adjCLITT = 0
  adjCSOMF = 0
  adjCSOMS = 0
  adjNLITT = 0
  adjNMIN  = 0
  adjNSOMF = 0
  adjNSOMS = 0
  adjWA    = 0
  do i = 1,nc
    do j = 1,nc
      adjCL(i)    = adjCL(i)    + dfAfromto(j,i) * (CL(j)    - CL(i)   )
      adjCP(i)    = adjCP(i)    + dfAfromto(j,i) * (CP(j)    - CP(i)   )  
      adjCR(i)    = adjCR(i)    + dfAfromto(j,i) * (CR(j)    - CR(i)   )
      adjCW(i)    = adjCW(i)    + dfAfromto(j,i) * (CW(j)    - CW(i)   )
      adjNL(i)    = adjNL(i)    + dfAfromto(j,i) * (NL(j)    - NL(i)   )
      adjLAI(i)   = adjLAI(i)   + dfAfromto(j,i) * (LAI(j)   - LAI(i)  )
      adjCLITT(i) = adjCLITT(i) + dfAfromto(j,i) * (CLITT(j) - CLITT(i))
      adjCSOMF(i) = adjCSOMF(i) + dfAfromto(j,i) * (CSOMF(j) - CSOMF(i))
      adjCSOMS(i) = adjCSOMS(i) + dfAfromto(j,i) * (CSOMS(j) - CSOMS(i))
      adjNLITT(i) = adjNLITT(i) + dfAfromto(j,i) * (NLITT(j) - NLITT(i))
      adjNMIN(i)  = adjNMIN(i)  + dfAfromto(j,i) * (NMIN(j)  - NMIN(i) )
      adjNSOMF(i) = adjNSOMF(i) + dfAfromto(j,i) * (NSOMF(j) - NSOMF(i))
      adjNSOMS(i) = adjNSOMS(i) + dfAfromto(j,i) * (NSOMS(j) - NSOMS(i))
      adjWA(i)    = adjWA(i)    + dfAfromto(j,i) * (WA(j)    - WA(i)   )
    enddo
  enddo
  end Subroutine CalcShade
  
end Module shade 
