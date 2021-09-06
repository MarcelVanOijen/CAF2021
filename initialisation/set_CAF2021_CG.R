## set_CAF2021_CG.R ##
## MvO, 2021-08-23

# 1. GENERAL INITIALISATION ##
  MODEL_dll <- 'CAF2021.DLL'
  dyn.load( MODEL_dll )
  source('initialisation/initialise_CAF2021_general.R')

# 2. COMMON SETTINGS for COSTA RICA & GUATEMALA ##
  year_start <- as.integer(2005)
  doy_start  <- as.integer(214)
  NDAYS      <- as.integer(15*365)
   
  file_params    <- 'parameters/parameters_default.txt'
    parcol       <- 31
  df_params      <- read.table( file_params, header=T, sep="\t", row.names=1 )
  names_params   <- row.names(df_params)
  params         <- df_params[,parcol]

  params <- set_par( c("PRUNTARGET(1)","PRUNTARGET(2)","PRUNTARGET(3)"),
                     c( 1             , 1             , 1             ), params )
  params <- set_par( c("THINTARGET(1)","THINTARGET(2)","THINTARGET(3)"),
                     c( 0             , 0             , 0             ), params )
  
  calendar_prunC[ 1:12, 1 ] <- 2009:2020
  calendar_prunC[ 1:12, 2 ] <- 59
  calendar_prunC[ 1:12, 3 ] <-  0.1

# 3. CREATE EMPTY MATRIX y FOR MODEL OUTPUT ##
  y <- matrix(0,NDAYS,NOUT)
