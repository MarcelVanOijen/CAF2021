module declare_parameters

! RUN CONTROL AND MATHEMATICAL CONSTANTS
integer, parameter :: DELT  = 1
real   , parameter :: PI    = ACOS(-1.)

! AGROFORESTRY SYSTEM  
integer, parameter :: nt    = 3         ! # shade tree species
integer, parameter :: ntlow = min(2,nt) ! # lower-stratum tree species 
integer, parameter :: nc    = 6         ! # land-cover classes (combinations of tree spp).
integer, parameter :: nz    = nt*2      ! # crown upper and lower heights for all tree spp.

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
real :: CAtreeMAX(nt), HMAX(nt), PRUNTARGET(nt), THINTARGET(nt), TREEDENS0(nt)
real :: NFERTMULT

! TREES
real :: CBTREE0(nt), CLTREE0(nt), CRTREE0(nt), CSTREE0(nt)
real :: FHARVBT(nt), FHARVLT(nt), FLTMAX(nt)
real :: FPT(nt), FST(nt), FTCLMINT(nt), FWT(nt)
real :: KAC(nt), KACEXP(nt), KH(nt), KHEXP(nt), KNFIX(nt)
real :: LAIMAXT(nt), LUEMAX(nt), NCLMAXT(nt), SLAT(nt)
real :: TBEFOREPT(nt), TCBT(nt), TCLMAXT(nt), TCRT(nt), TCST(nt)
real :: TOPTT(nt), TTOLT(nt)
real :: BETA, FNCLMINT
real :: GAMMA, KBA, KEXTT, KNMINT, KNUPTT
real :: KRAININTT, LAI0, NCPT, NCRT, NCWT, SHADEPROJ, SHADETARGET
real :: TCPTHARV, TDIFFMAX, TRANCOT, WOODDENS

! COFFEE
real :: KSINKPPAR
real :: CL0,CP0,CR0,CW0,DVS0
real :: CCONC,KEXT,KFLPAR,KNMIN,KNUPT,KRAININT,NCLMAX,FNCLMIN
real :: NCP,NCR,NCW,RAINdoyHI,RUBISC,SINKL,SINKPMAX,SINKR,SINKW,SLAMAX,FSLAMIN
real :: TBEFOREP,TCLMAX,FTCLMIN,TCR,TMATB,TMATT,TRANCO,YG
real :: NLAMAX, NLAMIN
real :: FSINKPMAX0, KSINKPMAX

! EXTRA
real :: CNSOM0MULT, CSOM0MULT, FWCWPMULT, WCSTMULT, SHADETARGETMULT
  
end module declare_parameters
