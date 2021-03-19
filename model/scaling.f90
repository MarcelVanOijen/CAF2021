Module scaling

use declare_parameters
implicit none

real :: TX(nt,nc)

Contains

  Subroutine CalcTX
  real :: TX36(3,6)=0
  TX36        = 0
  TX36(1,2  ) = 1 ; TX36(1,5) = 1
  TX36(2,3  ) = 1 ; TX36(2,6) = 1
  TX36(3,4:6) = 1
  TX          = TX36(1:nt,1:nc)
  end Subroutine CalcTX
  
  Subroutine CalcAtc(Ac,TX, At,Atc)
  real    :: Ac(nc), TX(nt,nc)
  real    :: At(nt), Atc(nt,nc)
  integer :: it
  do it=1,nt
    Atc(it,:) = TX(it,:) * Ac(:)
  enddo
  At = sum( Atc, dim=2 )
  end Subroutine CalcAtc
  
  Subroutine RescaleExt_t_tcc(x_t,At,Atc, x_tc,x_c)
  real    :: x_t(nt), At(nt), Atc(nt,nc)
  real    :: x_tc(nt,nc), x_c(nc)
  integer :: it
  x_tc = 0
  do it=1,nt
    where (Atc(it,:)>0.) x_tc(it,:) = x_t(it) / At(it)
  enddo
  x_c = sum( x_tc, dim=1 )
  end Subroutine RescaleExt_t_tcc
  
  Subroutine RescaleInt_c_t(x_c,At,Atc, x_t)
  real    :: x_c(nc), At(nt), Atc(nt,nc)
  real    :: x_t(nt)
  integer :: it
  do it=1,nt
    x_t(it) = sum( Atc(it,:) * x_c(:) ) / At(it)
  enddo  
  end Subroutine RescaleInt_c_t
  
end Module scaling 
