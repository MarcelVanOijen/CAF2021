Subroutine set_params(pa)

use declare_parameters
implicit none
! As long as the total number of parameters stays below 130, the next line need not be changed
real pa(130)

!%%%%%% Initial constants and parameters for coffee

!! COFFEE
! Structural parameters
KSINKPPAR  = pa(  1) !
! Initial constants
CL0        = pa(  2) ! % (kg C m-2)        :
CP0        = pa(  3) ! % (kg C m-2)        :
CR0	       = pa(  4) ! % (kg C m-2)        :
CW0        = pa(  5) ! % (kg C m-2)        :
DVS0       = pa(  6) ! % (-)               :
! Parameters
CCONC      = pa(  7) ! % (kg C kg-1 DM)    :
DAYSPLNOP  = pa(  8) ! % (d)               : 848-1213
DAYSPRNOP  = pa(  9) ! % (d)               :
FNCLMIN    = pa( 10) ! % (-)               : 0.3-0.7
FSLAMIN    = pa( 11) ! % (-)               : 0.55-0.70
FTCCLMIN   = pa( 12) ! % (-)               :
KEXT       = pa( 13) ! % (m2 m-2)          :
KFLPAR     = pa( 14) ! % (MJ-1 PAR)        :
KNMIN      = pa( 15) ! % (kg N m-2)        :
KNUPT	     = pa( 16) ! % (kg N kg-1 C d-1) : 
KRAININT   = pa( 17) ! % (mm [m2 m-2]-1)   :
NCLMAX     = pa( 18) ! % (kg N kg-1 C)     : 0.06-0.08
NCP        = pa( 19) ! % (kg N kg-1 C)     : 0.025-0.05
NCR	       = pa( 20) ! % (kg N kg-1 C)     : 0.035-0.06
NCW	       = pa( 21) ! % (kg N kg-1 C)     : 0.009-0.03
RAINdoyHI  = pa( 22) ! % (mm)              :
RUBISC     = pa( 23) ! % (g m-2)           : 0.5-1.5
SINKL	     = pa( 24) ! % (-)               :
SINKPMAX   = pa( 25) ! % (-)               :
SINKR      = pa( 26) ! % (-)               :
SINKW      = pa( 27) ! % (-)               :
SLAMAX     = pa( 28) ! % (m2 kg-1 C)       : 23-36
TCCLMAX    = pa( 29) ! % (d)               : 750-1200
TCCR       = pa( 30) ! % (d)               : 1000-3650
TMATB      = pa( 31) ! % (degC)            :
TMATT      = pa( 32) ! % (degC d)          : 2780-3370
TRANCO     = pa( 33) ! % (mm d-1)          :
YG         = pa( 34) ! % (kg C kg-1 C)     :

!! TREES: COMMON PARAMETERS
! Initial constants
CBtree0    = pa( 35) ! % (kg C tree-1)     : Gs: 0.72
CLtree0    = pa( 36) ! % (kg C tree-1)     : Gs: 0.23
CRtree0    = pa( 37) ! % (kg C tree-1)     : Gs: 1.28
CStree0    = pa( 38) ! % (kg C tree-1)     : Gs: 0.81
! Parameters
BETA       = pa( 39) ! % (-)               :
FB         = pa( 40) ! % (kg C kg-1 C)     :
FNCLMINT   = pa( 41) ! % (-)               : Ca: 0.7(0.6-0.8), Ep: 0.6(0.5-0.7), Gs: 0.6(0.5-0.7)
FS         = pa( 42) ! % (kg C kg-1 C)     :
FTCCLMINT  = pa( 43) ! % (-)               : {Ca,Gs,Ti}: low, Ed: medium, Ep: medium(non-pollarded)-high(poll.)
GAMMA      = pa( 44) ! % (kg C kg-1 C)     :
KEXTT      = pa( 45) ! % (m2 m-2)          : Ca: 0.32-0.72, Ed: 0.53-0.64, Ep: 0.50-0.87, Gs: low-0.345, Id: high
KNMINT     = pa( 46) ! % (kg N m-2)        :
KNUPTT     = pa( 47) ! % (kg N kg-1 C d-1) :
KRAININTT  = pa( 48) ! % (mm [m2 m-2]-1)   :
NCRT       = pa( 49) ! % (kg N kg-1 C)     : Gs: 0.035
NCWT       = pa( 50) ! % (kg N kg-1 C)     : branches: 0.02-0.03, stem (Egran): 0.003
SHADEPROJ  = pa( 51) ! % (m2 m-2)          :
TCCBT      = pa( 52) ! % (d)               : Gs: 1000, Ca: 7300
TCCLMAXT   = pa( 53) ! % (d)               : Ca: 730, Ep: 81+-15, Gs: 100
TCCRT      = pa( 54) ! % (d)               : Trop. trees: 3650, Ep: 5200, Ca: 7300
TDIFFMAX   = pa( 55) ! % (degC)            : Ed: 1.5-1.9, Ijin: 3.9, Ti: 2.4-4
TOPTT      = pa( 56) ! % (degC)            :
TRANCOT    = pa( 57) ! % (mm d-1)          :
TTOLT      = pa( 58) ! % (degC)            :
WOODDENS   = pa( 59) ! % (kg C m-3)        : Ca: 215(180-235), Ed: 250(205-315), Ep: 125(115-155), Gs: 225(175-270), Ti: 185(160-200)

!! SOIL
! Initial constants
CLITT0     = pa( 60) ! % (kg C m-2)        : Egran: 0.225
CNLITT0    = pa( 61) ! % (kg C kg-1 N)     : Acaciamangium: 19-30, Egran: 31-56, Ep: 12(11-20), Gs: 10-20, Iedul: 14-19
CNSOMF0    = pa( 62) ! % (kg C kg-1 N)     : SOM: 9-14
CNSOMS0    = pa( 63) ! % (kg C kg-1 N)     : SOM: 9-14
CSOM0      = pa( 64) ! % (kg C m-2)        : Variation 50-200%
FCSOMF0    = pa( 65) ! % (-)               : 0.50-0.75
NMIN0      = pa( 66) ! % (kg N m-2)        :
! Parameters
FLITTSOMF  = pa( 67) ! % (kg kg-1)         :
FSOMFSOMS  = pa( 68) ! % (kg kg-1)         :
FWCAD      = pa( 69) ! % (-)               : 
FWCFC      = pa( 70) ! % (-)               : 0.65
FWCWET     = pa( 71) ! % (-)               : 0.87
FWCWP      = pa( 72) ! % (-)               : 0.41
KNEMIT     = pa( 73) ! % (kg N kg-1 N d-1) :
KRUNOFF    = pa( 74)
RNLEACH    = pa( 75) ! % (kg N kg-1 N)     :
ROOTD      = pa( 76) ! % (m)               :
RRUNBULK   = pa( 77)
SLOPE      = pa( 78)
TCLITT     = pa( 79) ! % (d)               : CAF: 79, Coffee: ~50, Ep: 80(78-104), Euc: "slow", Gs: 60(33-70), Inga: 388(285-6000+)
TCSOMF     = pa( 80) ! % (d)               : TROP. AFS: 6000-12000
TCSOMS     = pa( 81) ! % (d)               : TROP. AFS: 18000-36500
WCST       = pa( 82) ! % (m3 m-3)          : 0.633(0.62-0.66)

! ATMOSPHERE
CO2A       = pa( 83)

! LOCATION
LAT        = pa( 84)

! MULTIPLIERS FOR SENSITIVITY ANALYSIS
IOMULT     = pa( 85)
NFERTMULT  = pa( 86)
RAINMULT   = pa( 87)
TPLUS      = pa( 88)

! COFFEE BIENNIALITY
FSINKPMAX0 = pa( 89)
KSINKPMAX  = pa( 90)

!! TREES: SPECIES-SPECIFIC PARAMETERS
! Initial constants & management
TREEDENS0  = pa( 91: 93)

!!!!!!!!!!!!!!!!!!!!!!!!!! REMOVE or REPLACE (parameter no longer needed)
THINMULT   = pa( 94: 96)
!!!!!!!!!!!!!!!!!!!!!!!!!!

! Parameters
FLMAX      = pa( 97: 99) ! (kg C kg-1 C)
KAC        = pa(100:102) ! (m2)
KACEXP     = pa(103:105) ! (-)
KH         = pa(106:108) ! (m)
KHEXP      = pa(109:111) ! (-)
KNFIX      = pa(112:114) ! (kg N kg-1 C)
LAIMAXT    = pa(115:117) ! (m2 m-2)
LUEMAX     = pa(118:120) ! (kg C MJ-1 PAR)
NCLMAXT    = pa(121:123) ! (kg N kg-1 C)
SLAT       = pa(124:126) ! (m2 kg-1 C)

! KAC     (1) Ep   : 15.8
! KACEXP  (1) Ep   : 0.55
! KH      (3) Ti   : 6.42             , Ed: 5.54-5.70
! KHEXP   (3) Ti   : 0.235            , Ed: 0.33-0.34
! NCLMAXT (1) Ep   : 0.10(0.09-0.12)  , Gs    : 0.09(0.085-0.10)
!         (2) Id   : 0.065(0.055-0.07), NONFIX: 0.05(0.035-0.09)
!         (3) Ca   : 0.085(0.08-0.09) , Eglob : 0.04(0.035-0.045)
! SLAT    (1) Ep   : 38(31-58)        , Gs: 40-50
!         (2) Id   : 31-47
!         (3) Eglob: 13-27            , Egran: 24                , Enit: 5-12

end Subroutine set_params
