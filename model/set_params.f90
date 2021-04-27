Subroutine set_params(pa)

use declare_parameters
implicit none
! As long as the total number of parameters stays below 150, the next line need not be changed
real pa(150)

!! COFFEE
! Initial constants
CL0        = pa(  1) ! (kg C m-2)        :
CP0        = pa(  2) ! (kg C m-2)        :
CR0	       = pa(  3) ! (kg C m-2)        :
CW0        = pa(  4) ! (kg C m-2)        :
DVS0       = pa(  5) ! (-)               :
! Parameters
CCONC      = pa(  6) ! (kg C kg-1 DM)    :
FNCLMIN    = pa(  7) ! (-)               : 0.3-0.7
FSLAMIN    = pa(  8) ! (-)               : 0.55-0.70
FTCLMIN    = pa(  9) ! (-)               :
KEXT       = pa( 10) ! (m2 m-2)          :
KFLPAR     = pa( 11) ! (MJ-1 PAR)        :
KNMIN      = pa( 12) ! (kg N m-2)        :
KNUPT	     = pa( 13) ! (kg N kg-1 C d-1) : 
KRAININT   = pa( 14) ! (mm [m2 m-2]-1)   :
KSINKPPAR  = pa( 15) !
NCLMAX     = pa( 16) ! (kg N kg-1 C)     : 0.06-0.08
NCP        = pa( 17) ! (kg N kg-1 C)     : 0.025-0.05
NCR	       = pa( 18) ! (kg N kg-1 C)     : 0.035-0.06
NCW	       = pa( 19) ! (kg N kg-1 C)     : 0.009-0.03
RAINdoyHI  = pa( 20) ! (mm)              :
RUBISC     = pa( 21) ! (g m-2)           : 0.5-1.5
SINKL	     = pa( 22) ! (-)               :
SINKPMAX   = pa( 23) ! (-)               :
SINKR      = pa( 24) ! (-)               :
SINKW      = pa( 25) ! (-)               :
SLAMAX     = pa( 26) ! (m2 kg-1 C)       : 23-36
TBEFOREP   = pa( 27) ! (d)               : 848-1213
TCLMAX     = pa( 28) ! (d)               : 750-1200
TCR        = pa( 29) ! (d)               : 1000-3650
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
FTCLMINT   = pa( 40) ! (-)               : {Ca,Gs,Ti}: low, Ed: medium, Ep: medium(non-pollarded)-high(poll.)
GAMMA      = pa( 41) ! (kg C kg-1 C)     :
KEXTT      = pa( 42) ! (m2 m-2)          : Ca: 0.32-0.72, Ed: 0.53-0.64, Ep: 0.50-0.87, Gs: low-0.345, Id: high
KNMINT     = pa( 43) ! (kg N m-2)        :
KNUPTT     = pa( 44) ! (kg N kg-1 C d-1) :
KRAININTT  = pa( 45) ! (mm [m2 m-2]-1)   :
NCPT       = pa( 46) ! (kg N kg-1 C)     :
NCRT       = pa( 47) ! (kg N kg-1 C)     : Gs: 0.035
NCWT       = pa( 48) ! (kg N kg-1 C)     : branches: 0.02-0.03, stem (Egran): 0.003
SHADEPROJ  = pa( 49) ! (m2 m-2)          :
TCLMAXT    = pa( 50) ! (d)               : Ca: 730, Ep: 81+-15, Gs: 100
TCPTHARV   = pa( 51) !
TDIFFMAX   = pa( 52) ! (degC)            : Ed: 1.5-1.9, Ijin: 3.9, Ti: 2.4-4
TOPTT      = pa( 53) ! (degC)            :
TRANCOT    = pa( 54) ! (mm d-1)          :
TTOLT      = pa( 55) ! (degC)            :
WOODDENS   = pa( 56) ! (kg C m-3)        : Ca: 215(180-235), Ed: 250(205-315), Ep: 125(115-155), Gs: 225(175-270), Ti: 185(160-200)

!! SOIL
! Initial constants
CLITT0     = pa( 57) ! (kg C m-2)        : Egran: 0.225
CNLITT0    = pa( 58) ! (kg C kg-1 N)     : Acaciamangium: 19-30, Egran: 31-56, Ep: 12(11-20), Gs: 10-20, Iedul: 14-19
CNSOMF0    = pa( 59) ! (kg C kg-1 N)     : SOM: 9-14
CNSOMS0    = pa( 60) ! (kg C kg-1 N)     : SOM: 9-14
CSOM0      = pa( 61) ! (kg C m-2)        : Variation 50-200%
FCSOMF0    = pa( 62) ! (-)               : 0.50-0.75
NMIN0      = pa( 63) ! (kg N m-2)        :
! Parameters
FLITTSOMF  = pa( 64) ! (kg kg-1)         :
FSOMFSOMS  = pa( 65) ! (kg kg-1)         :
FWCAD      = pa( 66) ! (-)               : 
FWCFC      = pa( 67) ! (-)               : 0.65
FWCWET     = pa( 68) ! (-)               : 0.87
FWCWP      = pa( 69) ! (-)               : 0.41
KNEMIT     = pa( 70) ! (kg N kg-1 N d-1) :
KRUNOFF    = pa( 71)
RNLEACH    = pa( 72) ! (kg N kg-1 N)     :
ROOTD      = pa( 73) ! (m)               :
RRUNBULK   = pa( 74)
SLOPE      = pa( 75)
TCLITT     = pa( 76) ! (d)               : CAF: 79, Coffee: ~50, Ep: 80(78-104), Euc: "slow", Gs: 60(33-70), Inga: 388(285-6000+)
TCSOMF     = pa( 77) ! (d)               : TROP. AFS: 6000-12000
TCSOMS     = pa( 78) ! (d)               : TROP. AFS: 18000-36500
WCST       = pa( 79) ! (m3 m-3)          : 0.633(0.62-0.66)

!! ATMOSPHERE
CO2A       = pa( 80)

!! LOCATION
LAT        = pa( 81)

!! MULTIPLIERS FOR SENSITIVITY ANALYSIS
IOMULT     = pa( 82)
NFERTMULT  = pa( 83)
RAINMULT   = pa( 84)
TPLUS      = pa( 85)

!! COFFEE BIENNIALITY
FSINKPMAX0 = pa( 86)
KSINKPMAX  = pa( 87)

!! TREES: SPECIES-SPECIFIC PARAMETERS
FLTMAX     = pa( 88: 90) ! (kg C kg-1 C)
FPT        = pa( 91: 93) ! (kg C kg-1 C)
FST        = pa( 94: 96) ! (kg C kg-1 C)
FWT        = pa( 97: 99) ! (kg C kg-1 C)
KAC        = pa(100:102) ! (m2)
KACEXP     = pa(103:105) ! (-)
KH         = pa(106:108) ! (m)
KHEXP      = pa(109:111) ! (-)
KNFIX      = pa(112:114) ! (kg N kg-1 C)
LAIMAXT    = pa(115:117) ! (m2 m-2)
LUEMAX     = pa(118:120) ! (kg C MJ-1 PAR)
NCLMAXT    = pa(121:123) ! (kg N kg-1 C)
SLAT       = pa(124:126) ! (m2 kg-1 C)
TBEFOREPT	 = pa(127:129) ! (d)
TCBT       = pa(130:132) ! (d)
TCRT       = pa(133:135) ! (d)
TCST       = pa(136:138) ! (d)
TREEDENS0  = pa(139:141) ! (m-2)

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
! TCBT    (1) Gs   : 1000
!         (3) Ca   : 7300
! TCRT    (-) Trop. trees: 3650
!         (1) Ep   : 5200
!         (3) Ca   : 7300

end Subroutine set_params
