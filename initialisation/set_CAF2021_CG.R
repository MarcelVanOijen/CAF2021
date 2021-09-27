## set_CAF2021_CG.R ##
## MvO, 2021-09-27



# 1. GENERAL INITIALISATION ##

  MODEL_dll <- 'CAF2021.DLL'
  dyn.load( MODEL_dll )
  source('initialisation/initialise_CAF2021_general.R')
  
  
  
# 2. CREATE EMPTY MATRIX y FOR MODEL OUTPUT ##
  
  y <- matrix(0,NDAYS,NOUT)
  
  
  
# 3. COMMON SETTINGS for COSTA RICA & GUATEMALA ##
  
  year_start <- as.integer(2005)
  doy_start  <- as.integer(214)
  NDAYS      <- as.integer(15*365)
   
  file_params    <- 'parameters/parameters_default.txt'
    parcol       <- 32
  df_params      <- read.table( file_params, header=T, sep="\t", row.names=1 )
  names_params   <- row.names(df_params)
  params         <- df_params[,parcol]
  
  params <- set_par( c("TCLITT"       ,"TCSOMF"       ,"TCSOMS"       ),
                     c( 730           , 7300          , 73000         ) )
  params <- set_par( c("FCSOMF0"      ,"RNLEACH"                      ),
                     c( 0.5           , 0.5                           ) )
  params <- set_par( c("PRUNTARGET(1)","PRUNTARGET(2)","PRUNTARGET(3)"),
                     c( 1             , 1             , 1             ) )
  params <- set_par( c("THINTARGET(1)","THINTARGET(2)","THINTARGET(3)"),
                     c( 0             , 0             , 0             ) )
  params <- set_par( "SHADETARGETMULT", 0.5 )
  
  calendar_prunC[ 1:12, 1 ] <- 2009:2020
  calendar_prunC[ 1:12, 2 ] <- 59
  calendar_prunC[ 1:12, 3 ] <-  0.1

  
  
# 4. FUNCTIONS FOR FARM- OR COUNTRY-SPECIFIC SETTINGS ##
  
  Tinventory_CG <- function( densE=0, densI=0, densB=0, densA=0,
                             densG=0, densC=0 ) {
    Tdens <- rep(0,3) ; Tnames <- rep("",3)
    if(densE>0)     { Tnames[1] <- "E" ; Tdens[1] <- densE }
    if(densI>densE) { Tnames[1] <- "I" ; Tdens[1] <- densI }
    if(densB>0)     { Tnames[2] <- "B" ; Tdens[2] <- densB }
    if(densA>densB) { Tnames[2] <- "A" ; Tdens[2] <- densA }
    if(densG>0)     { Tnames[3] <- "G" ; Tdens[3] <- densG }
    if(densC>densG) { Tnames[3] <- "C" ; Tdens[3] <- densC }
    if(densB==0 & densA==0 & densE>0 & densI>0) {
      Tnames[1] <- "E" ; Tdens[1] <- densE
      Tnames[2] <- "I" ; Tdens[2] <- densI }
    if(densE==0 & densI==0 & densB>0 & densA>0) {
      Tnames[1] <- "B" ; Tdens[1] <- densB
      Tnames[2] <- "A" ; Tdens[2] <- densA }
    return( list( Tnames=Tnames, Tdens=Tdens ) )
  }
  
  set_parT <- function( densE=0, densI=0, densB=0, densA=0,
                        densG=0, densC=0, p.old=params ) {
    p.new <- p.old
    d1    <- which( names_params=="TREEDENS0(1)" ) 
    d2    <- which( names_params=="TREEDENS0(2)" ) 
    d3    <- which( names_params=="TREEDENS0(3)" )
    p.new[ c(d1,d2,d3) ] <- 0
    
    if(densE>0)     { p.new[d1] <- densE/1e4
    p.new     <- set_par_speciesT(1,"Erythrina",p.new) }
    if(densI>densE) { p.new[d1] <- densI/1e4
    p.new     <- set_par_speciesT(1,"Inga"     ,p.new) }
    if(densB>0)     { p.new[d2] <- densB/1e4
    p.new     <- set_par_speciesT(2,"Banana"   ,p.new) }
    if(densA>densB) { p.new[d2] <- densA/1e4
    p.new     <- set_par_speciesT(2,"Avocado"  ,p.new) }
    if(densG>0)     { p.new[d3] <- densG/1e4
    p.new     <- set_par_speciesT(3,"Grevillea",p.new) }
    if(densC>densG) { p.new[d3] <- densC/1e4
    p.new     <- set_par_speciesT(3,"Cordia"   ,p.new) }
    if(densB==0 & densA==0 & densE>0 & densI>0) {
      p.new[d1] <- densE/1e4 ; p.new[d2] <- densI/1e4
      p.new     <- set_par_speciesT(1,"Erythrina",p.new)
      p.new     <- set_par_speciesT(2,"Inga"     ,p.new) }
    if(densE==0 & densI==0 & densB>0 & densA>0) {
      p.new[d1] <- densB/1e4 ; p.new[d2] <- densA/1e4
      p.new     <- set_par_speciesT(1,"Banana"   ,p.new)
      p.new     <- set_par_speciesT(2,"Avocado"  ,p.new) }
    return( p.new )
  }
  
  set_par_CG <- function( df.f=df_C.f, fi=1, p.old=params, country="C" ) {
    p.new <- p.old
    
    C     <- df.f$C.soil [ fi ]
    CN    <- df.f$CN.soil[ fi ]
    LAT   <- df.f$lat    [ fi ]
    SLOPE <- df.f$slope  [ fi ]
    shade <- df.f$shade  [ fi ]
    
    WCWP  <- df.f$WP     [ fi ]
    WCFC  <- df.f$FC     [ fi ]
    WCAD  <- 0.01
    WCST  <- min( 1, WCFC+0.1 )
    WCWET <- 0.5 * (WCFC + WCST)
    
    densE <- df.f$dens_Erythrina[ fi ]
    densI <- df.f$dens_Inga     [ fi ]
    densB <- df.f$dens_Banana   [ fi ]
    densA <- df.f$dens_Avocado  [ fi ]
    densG <- df.f$dens_Grevillea[ fi ]
    densC <- df.f$dens_Cordia   [ fi ]
    
    p.new <- set_parT( densE, densI, densB, densA, densG, densC, p.old=p.new )
    p.new <- set_par( c("CNLITT0","CNSOMF0","CNSOMS0"),
                      c( CN      , CN      , CN      ), p.new )
    p.new <- set_par( "CSOM0"      , C         , p.new )
    p.new <- set_par( "LAT"        , LAT       , p.new )
    p.new <- set_par( "SLOPE"      , SLOPE     , p.new )
    p.new <- set_par( "SHADETARGET", shade     , p.new )
    p.new <- set_par( "FWCAD"      , WCAD /WCST, p.new )
    p.new <- set_par( "FWCWP"      , WCWP /WCST, p.new )
    p.new <- set_par( "FWCFC"      , WCFC /WCST, p.new )
    p.new <- set_par( "FWCWET"     , WCWET/WCST, p.new )
    p.new <- set_par( "WCST"       , WCST      , p.new )
    
    p.new <- set_par( c("FHARVLT(1)","FHARVLT(2)","FHARVLT(3)"), c(0,0,0), p.new )
    if(country %in% c("C", "C.", "C.R.", "CRI", "CostaRica", "Costa Rica")) {
      p.new <- set_par( c("FHARVBT(1)","FHARVBT(2)","FHARVBT(3)"), c(0,0,0), p.new )
      if(densE>0){ p.new <- set_par( "HMAX(1)"   ,  6, p.new ) }
      if(densA>0){ p.new <- set_par( "HMAX(2)"   ,  6, p.new ) }
    } else if(country %in% c("G", "G.", "GTM", "Guatemala")) {
      p.new <- set_par( c("FHARVBT(1)","FHARVBT(2)","FHARVBT(3)"), c(1,1,1), p.new )
      if(densE>0){ p.new <- set_par( "FHARVBT(1)",  0, p.new )
      p.new <- set_par( "HMAX(1)"   , 10, p.new ) }
      if(densA>0){ p.new <- set_par( "HMAX(2)"   , 10, p.new ) }
    } else { stop( paste("Parameterisation for", country, "not provided.") ) }
    
    return( p.new )
  }
  
  set_fert_CG <- function( df.f=df_C.f, fi=1 ) {
    Nfert <- df.f$Nfert[ fi ]
    cf    <- matrix( -1, nrow=100, ncol=3 )
    
    cf [ 1:57, 1 ] <- rep( 2002:2020, each=3 )
    cf [ 1:57, 2 ] <- c( 135, 289, 350 )
    cf [ 1:57, 3 ] <- rep( Nfert/3, 3 )
    return( cf )
  }
  
  set_prunT_CG <- function( df.f=df_C.f, fi=1 ) {
    densE <- df.f$dens_Erythrina[ fi ]
    densI <- df.f$dens_Inga     [ fi ]
    densG <- df.f$dens_Grevillea[ fi ]
    densC <- df.f$dens_Cordia   [ fi ]
    
    cp <- array( -1, c(3,100,3) )
    yE <- rep( 2008:2020, each=2 ) ; nE <- length( yE )
    dE <- rep( c(140,350), nE/2 )
    fE <- rep( 0.9, nE )
    yI <- yG <- yC <- 2008:2020 ; nI <- nG <- nC <- length( yC )
    dI <- dG <- dC <- rep(140,nI) 
    fI <- fG <- fC <- rep( 0.1, nI )
    
    if(densE>0) {
      cp[1,1:nE,1] <- yE ; cp[1,1:nE,2] <- dE ; cp[1,1:nE,3] <- fE }
    if(densI>0 & densE==0) {
      cp[1,1:nI,1] <- yI ; cp[1,1:nI,2] <- dI ; cp[1,1:nI,3] <- fI }
    if(densI>0 & densE >0) {
      cp[2,1:nI,1] <- yI ; cp[2,1:nI,2] <- dI ; cp[2,1:nI,3] <- fI }
    if(densG>0) {
      cp[3,1:nG,1] <- yG ; cp[3,1:nG,2] <- dG ; cp[3,1:nG,3] <- fG }
    # Comment out next two lines if timber-trees like Cordia are not pruned
    if(densC>0) {
      cp[3,1:nC,1] <- yC ; cp[3,1:nC,2] <- dC ; cp[3,1:nC,3] <- fC }
    return( cp )
  }
  
  set_thinT_CG <- function( df.f=df_C.f, fi=1 ) {
    densC <- df.f$dens_Cordia[ fi ]
    
    ct <- array( -1, c(3,100,3) )
    yC <- seq(2010,2020,by=5) ; dC <- 124 ; fC <- 0.3 ; nC <- length( yC )
    
    if(densC>0) {
      ct[3,1:nC,1] <- yC ; ct[3,1:nC,2] <- dC ; ct[3,1:nC,3] <- fC }
    return( ct )
  }
  