## set_CAF2021_Turrialba.R ##
## MvO, 2021-06-04

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
	
## 4. PARAMETERISATION & COFFEE ##
   df_params      <- read.table( file_params, header=T, sep="\t", row.names=1 )
   names_params   <- row.names(df_params)
   params         <- df_params[,parcol]

   calendar_prunC[ 1:14, 1 ] <- 2004:2017
   calendar_prunC[ 1:14, 2 ] <- 59
   calendar_prunC[ 1:14, 3 ] <-  0.28
   
## 5. CREATE EMPTY MATRIX y FOR MODEL OUTPUT ##
   y <- matrix(0,NDAYS,NOUT)

## 6. TREES & FERTILISATION
