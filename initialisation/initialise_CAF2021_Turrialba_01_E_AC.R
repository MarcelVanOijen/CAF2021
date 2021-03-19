## initialise_CAF2021_Turrialba_01_E_AC.R ##
## MvO, 2021-03-19

## 1. SITE-SPECIFIC SETTINGS ##
   year_start     <- as.integer(2000)
   doy_start      <- as.integer( 214)
   NDAYS          <- as.integer(6357)
   file_weather   <- 'weather/weather_Turrialba_2000_2017.txt'
   file_params    <- 'parameters/parameters_default.txt'
     parcol       <- 1

## 2. GENERAL INITIALISATION ##
   MODEL_dll      <- 'CAF2021.DLL'
   dyn.load( MODEL_dll )
   source('initialisation/initialise_CAF2021_general.R')

## 3. WEATHER
   matrix_weather <- read_weather_CAF( year_start, doy_start, NDAYS, file_weather )
	
## 4. PARAMETERISATION AND MANAGEMENT ##
   df_params      <- read.table( file_params, header=T, sep="\t", row.names=1 )
   names_params <- row.names(df_params)
   params       <- df_params[,parcol]
   params[names_params=="TREEDENS0(1)"] <- 0.07

   calendar_fert [ 1:48,1] <- rep( 2002:2017, each=3 )
   calendar_fert [ 1:48,2] <- c( 135,289, 350 )
   calendar_fert [ 1:48,3] <- c(  90, 90, 100 )

   calendar_prunC[ 1:14,1] <- 2004:2017
   calendar_prunC[ 1:14,2] <- 59
   calendar_prunC[ 1:14,3] <-  0.28

   calendar_prunT[    1, ] <- c( 2001, 266, 0.8 )
   calendar_prunT[ 2:33,1] <- rep( 2002:2017, each=2 )
   calendar_prunT[ 2:33,2] <- c( 140, 350 )
   calendar_prunT[ 2:33,3] <-  0.6
   
   calendar_thinT[    1, ] <- c( 2007, 124, 0.004 )
   calendar_thinT[    2, ] <- c( 2010, 124, 0.49  )
   
## 5. CREATE EMPTY MATRIX y FOR MODEL OUTPUT ##
   y <- matrix(0,NDAYS,NOUT)
