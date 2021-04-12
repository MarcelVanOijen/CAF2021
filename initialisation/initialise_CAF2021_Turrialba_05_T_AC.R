## initialise_CAF2021_Turrialba_05_T_AC.R ##
## MvO, 2021-04-12

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
   df_params    <- read.table( file_params, header=T, sep="\t", row.names=1 )
   names_params <- row.names(df_params)
   params       <- df_params[,parcol]
   params[names_params=="TREEDENS0(3)"] <- 0.07
   
   calendar_fert [ 1:48, 1 ] <- rep( 2002:2017, each=3 )
   calendar_fert [ 1:48, 2 ] <- c( 135, 289, 350 )
   calendar_fert [ 1:48, 3 ] <- c(  90,  90, 100 )

   calendar_prunC[ 1:14, 1 ] <- 2004:2017
   calendar_prunC[ 1:14, 2 ] <- 59
   calendar_prunC[ 1:14, 3 ] <-  0.28

   calendar_prunT[ 3,  1, ] <- c( 2003, 158, 0.1  )
   calendar_prunT[ 3,  2, ] <- c( 2004,  49, 0.03 )
   calendar_prunT[ 3,  3, ] <- c( 2004, 175, 0.03 )
   calendar_prunT[ 3,  4, ] <- c( 2004, 320, 0.03 )
   calendar_prunT[ 3,  5, ] <- c( 2005, 220, 0.1  )
   calendar_prunT[ 3,  6, ] <- c( 2006, 220, 0.1  )
   calendar_prunT[ 3,  7, ] <- c( 2007, 220, 0.1  )
   calendar_prunT[ 3,  8, ] <- c( 2008, 220, 0.1  )
   calendar_prunT[ 3,  9, ] <- c( 2010, 220, 0.1  )
   calendar_prunT[ 3, 10, ] <- c( 2011, 220, 0.1  )
   calendar_prunT[ 3, 11, ] <- c( 2012, 250, 0.1  )
   calendar_prunT[ 3, 12, ] <- c( 2013, 226, 0.1  )
   calendar_prunT[ 3, 13, ] <- c( 2014, 226, 0.1  )
   calendar_prunT[ 3, 14, ] <- c( 2015, 226, 0.1  )
   calendar_prunT[ 3, 15, ] <- c( 2016, 226, 0.1  )
   calendar_prunT[ 3, 16, ] <- c( 2017, 226, 0.1  )

   calendar_thinT[ 3,  1, ] <- c( 2007,  43, 0.25 )
   calendar_thinT[ 3,  2, ] <- c( 2008,  72, 0.35 )
   calendar_thinT[ 3,  3, ] <- c( 2009,  89, 0.28 )
   calendar_thinT[ 3,  4, ] <- c( 2011,  75, 0.24 )
   calendar_thinT[ 3,  5, ] <- c( 2012,  80, 0.25 )

## 5. CREATE EMPTY MATRIX y FOR MODEL OUTPUT ##
   y <- matrix(0,NDAYS,NOUT)
