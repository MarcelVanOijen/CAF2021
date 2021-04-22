Subroutine set_params(pa)

use declare_parameters
implicit none
! As long as the total number of parameters stays below 140, the next line need not be changed
real pa(140)

!! COFFEE
! Initial constants
CL0        = pa(  1) ! (kg C m-2)        :
CP0        = pa(  2) ! (kg C m-2)        :
CR0	       = pa(  3) ! (kg C m-2)        :
CW0        = pa(  4) ! (kg C m-2)        :
DVS0       = pa(  5) ! (-)               :
! Parameters
CCONC      = pa(  6) ! (kg C kg-1 DM)    :
DAYSPLNOP  = pa(  7) ! (d)               : 848-1213
FNCLMIN    = pa(  8) ! (-)               : 0.3-0.7
FSLAMIN    = pa(  9) ! (-)               : 0.55-0.70
FTCCLMIN   = pa( 10) ! (-)               :
KEXT       = pa( 11) ! (m2 m-2)          :
KFLPAR     = pa( 12) ! (MJ-1 PAR)        :
KNMIN      = pa( 13) ! (kg N m-2)        :
KNUPT	     = pa( 14) ! (kg N kg-1 C d-1) : 
KRAININT   = pa( 15) ! (mm [m2 m-2]-1)   :
KSINKPPAR  = pa( 16) !
NCLMAX     = pa( 17) ! (kg N kg-1 C)     : 0.06-0.08
NCP        = pa( 18) ! (kg N kg-1 C)     : 0.025-0.05
NCR	       = pa( 19) ! (kg N kg-1 C)     : 0.035-0.06
NCW	       = pa( 20) ! (kg N kg-1 C)     : 0.009-0.03
RAINdoyHI  = pa( 21) ! (mm)              :
RUBISC     = pa( 22) ! (g m-2)           : 0.5-1.5
SINKL	     = pa( 23) ! (-)               :
SINKPMAX   = pa( 24) ! (-)               :
SINKR      = pa( 25) ! (-)               :
SINKW      = pa( 26) ! (-)               :
SLAMAX     = pa( 27) ! (m2 kg-1 C)       : 23-36
TCCLMAX    = pa( 28) ! (d)               : 750-1200
TCCR       = pa( 29) ! (d)               : 1000-3650
TMATB      = pa( 30) ! (degC)            :
TMATT      = pa( 31) ! (degC d)          : 2780-3370
TRANCO     = pa( 32) ! (mm d-1)          :
YG         = pa( 33) ! (kg C kg-1 C)     :

!! TREES: SHARED PARAMETERS FOR ALL SPECIES
! Initial constants
CBtree0    = pa( 34) ! (kg C tree-1)     : Gs: 0.72
CLtree0    = pa( 35) ! (kg C tree-1)     : Gs: 0.23
CRtree0    = pa( 36) ! (kg C tree-1)     : Gs: 1.28
CStree0    = pa( 37) ! (kg C tree-1)     : Gs: 0.81
! Parameters
BETA       = pa( 38) ! (-)               :
FNCLMINT   = pa( 39) ! (-)               : Ca: 0.7(0.6-0.8), Ep: 0.6(0.5-0.7), Gs: 0.6(0.5-0.7)
FTCCLMINT  = pa( 40) ! (-)               : {Ca,Gs,Ti}: low, Ed: medium, Ep: medium(non-pollarded)-high(poll.)
GAMMA      = pa( 41) ! (kg C kg-1 C)     :
KEXTT      = pa( 42) ! (m2 m-2)          : Ca: 0.32-0.72, Ed: 0.53-0.64, Ep: 0.50-0.87, Gs: low-0.345, Id: high
KNMINT     = pa( 43) ! (kg N m-2)        :
KNUPTT     = pa( 44) ! (kg N kg-1 C d-1) :
KRAININTT  = pa( 45) ! (mm [m2 m-2]-1)   :
NCPT       = pa( 46) ! (kg N kg-1 C)     :
NCRT       = pa( 47) ! (kg N kg-1 C)     : Gs: 0.035
NCWT       = pa( 48) ! (kg N kg-1 C)     : branches: 0.02-0.03, stem (Egran): 0.003
SHADEPROJ  = pa( 49) ! (m2 m-2)          :
TCCBT      = pa( 50) ! (d)               : Gs: 1000, Ca: 7300
TCCLMAXT   = pa( 51) ! (d)               : Ca: 730, Ep: 81+-15, Gs: 100
TCCRT      = pa( 52) ! (d)               : Trop. trees: 3650, Ep: 5200, Ca: 7300
TDIFFMAX   = pa( 53) ! (degC)            : Ed: 1.5-1.9, Ijin: 3.9, Ti: 2.4-4
TOPTT      = pa( 54) ! (degC)            :
TRANCOT    = pa( 55) ! (mm d-1)          :
TTOLT      = pa( 56) ! (degC)            :
WOODDENS   = pa( 57) ! (kg C m-3)        : Ca: 215(180-235), Ed: 250(205-315), Ep: 125(115-155), Gs: 225(175-270), Ti: 185(160-200)

!! SOIL
! Initial constants
CLITT0     = pa( 58) ! (kg C m-2)        : Egran: 0.225
CNLITT0    = pa( 59) ! (kg C kg-1 N)     : Acaciamangium: 19-30, Egran: 31-56, Ep: 12(11-20), Gs: 10-20, Iedul: 14-19
CNSOMF0    = pa( 60) ! (kg C kg-1 N)     : SOM: 9-14
CNSOMS0    = pa( 61) ! (kg C kg-1 N)     : SOM: 9-14
CSOM0      = pa( 62) ! (kg C m-2)        : Variation 50-200%
FCSOMF0    = pa( 63) ! (-)               : 0.50-0.75
NMIN0      = pa( 64) ! (kg N m-2)        :
! Parameters
FLITTSOMF  = pa( 65) ! (kg kg-1)         :
FSOMFSOMS  = pa( 66) ! (kg kg-1)         :
FWCAD      = pa( 67) ! (-)               : 
FWCFC      = pa( 68) ! (-)               : 0.65
FWCWET     = pa( 69) ! (-)               : 0.87
FWCWP      = pa( 70) ! (-)               : 0.41
KNEMIT     = pa( 71) ! (kg N kg-1 N d-1) :
KRUNOFF    = pa( 72)
RNLEACH    = pa( 73) ! (kg N kg-1 N)     :
ROOTD      = pa( 74) ! (m)               :
RRUNBULK   = pa( 75)
SLOPE      = pa( 76)
TCLITT     = pa( 77) ! (d)               : CAF: 79, Coffee: ~50, Ep: 80(78-104), Euc: "slow", Gs: 60(33-70), Inga: 388(285-6000+)
TCSOMF     = pa( 78) ! (d)               : TROP. AFS: 6000-12000
TCSOMS     = pa( 79) ! (d)               : TROP. AFS: 18000-36500
WCST       = pa( 80) ! (m3 m-3)          : 0.633(0.62-0.66)

!! ATMOSPHERE
CO2A       = pa( 81)

!! LOCATION
LAT        = pa( 82)

!! MULTIPLIERS FOR SENSITIVITY ANALYSIS
IOMULT     = pa( 83)
NFERTMULT  = pa( 84)
RAINMULT   = pa( 85)
TPLUS      = pa( 86)

!! COFFEE BIENNIALITY
FSINKPMAX0 = pa( 87)
KSINKPMAX  = pa( 88)

!! TREES: SPECIES-SPECIFIC PARAMETERS
FLTMAX     = pa( 89: 91) ! (kg C kg-1 C)
FPT        = pa( 92: 94) ! (kg C kg-1 C)
FST        = pa( 95: 97) ! (kg C kg-1 C)
FWT        = pa( 98:100) ! (kg C kg-1 C)
KAC        = pa(101:103) ! (m2)
KACEXP     = pa(104:106) ! (-)
KH         = pa(107:109) ! (m)
KHEXP      = pa(110:112) ! (-)
KNFIX      = pa(113:115) ! (kg N kg-1 C)
LAIMAXT    = pa(116:118) ! (m2 m-2)
LUEMAX     = pa(119:121) ! (kg C MJ-1 PAR)
NCLMAXT    = pa(122:124) ! (kg N kg-1 C)
SLAT       = pa(125:127) ! (m2 kg-1 C)\
TREEDENS0  = pa(128:130) ! (m-2)

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
