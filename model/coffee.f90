module coffee

! Subroutines: Phenology, Growth, Foliage, Senescence, PrunHarv

use declare_parameters
use environment
use management
use shade
implicit none

real :: gCW(nc),gCP(nc),gCL(nc),gNL(nc),gCR(nc),gLAI(nc),gSINKP(nc)
real :: dCL(nc),dCR(nc),dLAI(nc),dNL(nc),Nupt(nc)
real :: prunLAI(nc),prunNL(nc),prunCL(nc),prunCW(nc),harvCP(nc),harvNP(nc)
! real :: dSENSIT(nc),dDVS(nc),rDVS(nc),DayFl,DayHarv(nc)
real :: dDVS(nc),rDVS(nc),DayFl,DayHarv(nc)
real :: RAINdoy, SINKPMAXnew(nc)

Contains

  Subroutine Phenology(doy,DVS,SENSIT,TCOFFEE,SINKP)
  integer :: doy
  integer :: DayFill(nc)
  real    :: DVS(:), SENSIT, TCOFFEE(:), SINKP(:)
! Days with bean filling
  where ((DVS>0.).and.(DVS<1.))
    DayFill = 1  
  elsewhere
    DayFill = 0
  endwhere
! Triggering flowering
  RAINdoy = RAIN * doy
  if ((RAINdoy>RAINdoyHI).and.(SENSIT==1)) then
    DayFl = 1
  else
    DayFl = 0
  endif
! Harvest days
  where (((DayFl==1).and.(DayFill==1)).or.((DayFl==0).and.(DVS>=1.)))
    DayHarv     = 1
    rDVS        = DVS / DELT
	  SINKPMAXnew = SINKPMAX - KSINKPMAX * SINKP
  elsewhere
    DayHarv     = 0
    rDVS        = 0
  endwhere
! Development rate
  where ((DayFl==1).or.(DayFill==1))
    dDVS = max(0., min(1., (TCOFFEE - TMATB) / TMATT ) )
  elsewhere
    dDVS = 0
  endwhere
  end Subroutine Phenology

  Subroutine Growth(TINP,PARav,PARint,fTran,SINKP,Nsup,PARMA,DVS,fNgrowth)
  real :: TINP(:),PARav(:),PARint(:),fTran(:),SINKP(:),Nsup(:),DVS(:),fNgrowth(nc)
  real :: EAVCMX,EAKMC,EAKMO,JMUMOL,KC25,KMC25,KMO25,KOKC,O2,R
  real :: CO2I,VCMAX(nc),KMC(nc),KMO(nc),GAMMAX(nc),PMAX(nc),EFF(nc)
  real :: LUECO2(nc),CCass(nc),gSHsource(nc)
  real :: SINKSUM(nc)
  real :: PARMA(nc),Ndemand(nc)
  real :: FCL(nc),FCW(nc),FCR(nc),FCP(nc),gCLpot(nc),gCWpot(nc),gCRpot(nc)
  real :: gCPpot(nc),gNLpot(nc),gNWpot(nc),gNRpot(nc),gNPpot(nc),gNLmax(nc)
  real :: gNW(nc),gNR(nc),gNP(nc),NCLnew(nc)
  ! Source strength
  EAVCMX =  68000.                                                  ! % (J mol-1)
  EAKMC  =  65800.                                                  ! % (J mol-1)
  EAKMO  =   1400.                                                  ! % (J mol-1)
  JMUMOL =      4.56                                                ! % (mol quanta MJ-1 PAR)
  KC25   =    138.                                                  ! % (g CO2 g-1 Rubisco d-1)
  KMC25  =    460.                                                  ! % (ppm CO2)
  KMO25  =     33.                                                  ! % (% O2)
  KOKC   =      0.21                                                ! % (-)
  O2     =     21.                                                  ! % (% O2)
  R      =      8.314                                               ! % (J K-1 mol-1)
  CO2I   = 0.7 * CO2A                                               ! % (ppm CO2)
  VCMAX  = RUBISC * KC25 * exp((1./298.-1./(TINP+273.))*EAVCMX/R)   ! % (g CO2 m-2 leaf d-1)
  KMC    =         KMC25 * exp((1./298.-1./(TINP+273.))*EAKMC /R)   ! % (ppm CO2)
  KMO    =         KMO25 * exp((1./298.-1./(TINP+273.))*EAKMO /R)   ! % (% O2)
  GAMMAX = 0.5 * KOKC * KMC * O2 / KMO                              ! % (ppm CO2)
  PMAX   = VCMAX * (CO2I-GAMMAX) / (CO2I + KMC * (1+O2/KMO))        ! % (g CO2 m-2 leaf d-1)
  EFF    = 44. * JMUMOL/2.1 * (CO2I-GAMMAX)/ (4.5*CO2I+10.5*GAMMAX) ! % (g CO2 MJ-1 PAR)
  LUECO2 = EFF * PMAX / (EFF*KEXT*PARav + PMAX)                     ! % (g CO2 MJ-1 PAR)
  CCass  = LUECO2*0.001*(12./44.) * PARint * fTran
  gSHsource = CCass * YG
  ! Sink strength
!  gSINKP   = (1 - exp(-KSINKPPAR * PARMA)) * DayFl * SINKPMAXnew * fTran * fNgrowth 
  gSINKP   = (1 - exp(-KSINKPPAR * PARMA)) * DayFl * SINKPMAXnew
  SINKSUM  = SINKL + SINKW + SINKR * (2-fTran) + SINKP * min(1.,2*DVS)
  FCL      = SINKL                 / SINKSUM
  FCW      = SINKW                 / SINKSUM
  FCR      = SINKR * (2-fTran)     / SINKSUM
  FCP      = SINKP * min(1.,2*DVS) / SINKSUM  
  ! N-demand Organs
  gCLpot   = FCL * gSHsource
  gCWpot   = FCW * gSHsource
  gCRpot   = FCR * gSHsource
  gCPpot   = FCP * gSHsource
  gNLpot   = gCLpot * NCLMAX
  gNWpot   = gCWpot * NCW
  gNRpot   = gCRpot * NCR
  gNPpot   = gCPpot * NCP    
  Ndemand  = gNLpot + gNWpot + gNRpot + gNPpot
  Nupt     = min( Nsup, Ndemand )
  where (Ndemand>0.)
    fNgrowth = max( 0., min( 1., Nupt / Ndemand ) )
  elsewhere 
    fNgrowth = 0.
  endwhere
  gNLmax   = fNgrowth * gNLpot
  gNP      = fNgrowth * gNPpot
  gNR      = fNgrowth * gNRpot
  gNW      = fNgrowth * gNWpot
  gNL      = fNgrowth * gNLmax
  NCLnew   = (fNCLmin + fNgrowth*(1. - fNCLmin)) * NCLmax 
  gCW      = gNW / NCW
  gCP      = gNP / NCP
  gCR      = (gNR + (gNLmax - gNL)) / NCR
  gCL      = min( gNL/NCLnew, gCLpot )
  end Subroutine Growth  
  
  Subroutine Foliage(fTran)
  real :: fTran(nc)
  real :: SLA(nc)
  SLA  = SLAMAX * (FSLAMIN + fTran*(1- FSLAMIN))
  gLAI = SLA * gCL
  end Subroutine Foliage
  
  Subroutine Senescence(CR,NL,CL,LAI,fTran)
  real CR(:),NL(:),CL(:),LAI(:),fTran(:) 
  real KdL(nc)
  KdL  = max(0.,min(1., 1 / ((fTran+FTCLMIN*(1.-fTran))*TCLMAX) ))
  dNL  = NL  * KdL
  dCL  = CL  * KdL
  dLAI = LAI * KdL
  dCR  = CR  / TCR  
  end Subroutine Senescence  
 
  Subroutine PrunHarv(NL,CL,CW,CP,LAI,DVS)
  real NL(:),CL(:),CW(:),CP(:),LAI(:),DVS(:)
  prunLAI = prunFRC * LAI / DELT
  prunNL  = prunFRC * NL  / DELT
  prunCL  = prunFRC * CL  / DELT
  prunCW  = prunFRC * CW  / DELT  
!  where (DVS>=1.)
  where (DayHarv==1.)
    harvCP = CP / DELT
    harvNP = harvCP * NCP  
!    adjCP  = 0.
  elsewhere
    harvCP = 0.
    harvNP = 0.
  endwhere
  end Subroutine PrunHarv

end module coffee
