module declare_parameters

! RUN CONTROL AND MATHEMATICAL CONSTANTS
integer, parameter :: DELT  = 1
real   , parameter :: PI    = ACOS(-1.)

! AGROFORESTRY SYSTEM  
integer, parameter :: nt    = 3         ! # shade tree species
integer, parameter :: ntlow = min(2,nt) ! # lower-stratum tree species 
integer, parameter :: nc    = 6         ! # land-cover classes (combinations of tree spp).

! ENVIRONMENT
real , parameter :: CO20 = 350 ! (ppm) Reference value of [CO2] at which fLUECO2 = 1
real             :: CO2A       ! (ppm) Actual value of [CO2]
real             :: LAT
real             :: IOMULT, RAINMULT, TPLUS

! SOIL
real :: KRUNOFF, RRUNBULK, SLOPE
real :: FWCAD, FWCWP, FWCFC, FWCWET, WCST
real :: CLITT0, CSOM0, CNLITT0, CNSOMF0, CNSOMS0, FCSOMF0, FLITTSOMF, FSOMFSOMS
real :: RNLEACH, KNEMIT, NMIN0, TCLITT, TCSOMF, TCSOMS, TMAXF, TSIGMAF, RFN2O
real :: ROOTD, WFPS50N2O
 
! MANAGEMENT
!NEXT LINE: Remove THINMULT
real :: THINMULT(nt), TREEDENS0(nt)! Remove THINMULT
real :: NFERTMULT

! TREES
real :: FLTMAX(nt), KAC(nt), KACEXP(nt), KH(nt), KHEXP(nt), KNFIX(nt)
real :: LAIMAXT(nt), LUEMAX(nt), NCLMAXT(nt), SLAT(nt)
real :: BETA, CBTREE0, CLTREE0, CRTREE0, CSTREE0, FWT, FNCLMINT, FST
real :: FTCCLMINT, GAMMA, KBA, KEXTT, KNMINT, KNUPTT
real :: KRAININTT, LAI0, NCRT, NCWT, SHADEPROJ
real :: TCCBT, TCCLMAXT, TCCRT, TDIFFMAX, TOPTT
real :: TTOLT, TRANCOT, WOODDENS

! COFFEE
real :: KSINKPPAR
real :: CL0,CP0,CR0,CW0,DVS0
!NEXT LINE: remove DAYSPRNOP
real :: CCONC,DAYSPLNOP,DAYSPRNOP,KEXT,KFLPAR,KNMIN,KNUPT,KRAININT,NCLMAX,FNCLMIN
real :: NCP,NCR,NCW,RAINdoyHI,RUBISC,SINKL,SINKPMAX,SINKR,SINKW,SLAMAX,FSLAMIN
real :: TCCLMAX,FTCCLMIN,TCCR,TMATB,TMATT,TRANCO,YG
real :: NLAMAX, NLAMIN
real :: FSINKPMAX0, KSINKPMAX
  
end module declare_parameters
