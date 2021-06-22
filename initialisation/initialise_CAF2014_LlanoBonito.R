## initialise_CAF2014_Turrialba_1.R ##

## 1. SITE-SPECIFIC SETTINGS ##
   year_start     <- as.integer(1993)
   doy_start      <- as.integer( 214)
   NDAYS          <- as.integer(4531)
   file_weather   <- 'weather/weather_LB_Old_1993_2005.txt'
   file_params    <- 'parameters/parameters_default.txt'
     parcol       <- 21

## 2. GENERAL INITIALISATION ##
   MODEL_dll      <- 'CAF2014.DLL'
   dyn.load( MODEL_dll )
   source('initialisation/initialise_CAF2014_general.R')

## 3. WEATHER
   matrix_weather <- read_weather_CAF( year_start, doy_start, NDAYS, file_weather )
	
## 4. PARAMETERISATION AND MANAGEMENT ##
   df_params      <- read.table( file_params, header=T, sep="\t", row.names=1 )
   params         <- df_params[,parcol]
   
   calendar_fert [1:33,1] <- rep( 1995:2005, each=3 )
   calendar_fert [1:33,2] <- c(15,136,228)
   calendar_fert [1:33,3] <- c(90, 90, 100)

  
  calendar_prunC[1:8,1] <- 1998:2005
  calendar_prunC[1:8,2] <- 59
  calendar_prunC[1:8,3] <- 0.28

   #calendar_prunT[1   , ] <- c(2003,158,0.4)
   #calendar_prunT[2   , ] <- c(2004,49,0.13)
   #calendar_prunT[3   , ] <- c(2004,175,0.13)
   #calendar_prunT[4   , ] <- c(2004,320,0.13)
   #calendar_prunT[5   , ] <- c(2005,230,0.4)
   #calendar_prunT[6   , ] <- c(2006,96,0.4)
   #calendar_prunT[7   , ] <- c(2007,46,0.4)
   #calendar_prunT[8   , ] <- c(2008,179,0.4)
   #calendar_prunT[9   , ] <- c(2010,112,0.4)
   
  calendar_thinT[1   , ] <- c( 1995,270, 0.2 )
   calendar_thinT[1   , ] <- c( 1998,124, 0.004 )
   #calendar_thinT[2   , ] <- c( 2003,124, 0.49 )
   #calendar_thinT[2   , ] <- c( 2008,72, 0.1 )
   #calendar_thinT[3   , ] <- c( 2009,89, 0.1 )
   #calendar_thinT[4   , ] <- c( 2011,75, 0.1 )
   
   calendar_prunT[  1, ] = c(1994,40,0.6)
   calendar_prunT[2:23,1] = rep(1995:2005, each=2)
   calendar_prunT[2:23,2] = c(182,288)
   calendar_prunT[2:23,3] = 0.3
      
#   calendar_prunC[1   , ] <- c(2000,182,0.95)
#   calendar_prunC[2   , ] <- c(2002,182,0.95)
#   calendar_prunC[3   , ] <- c(2004,182,0.95)

    
   
## 5. CREATE EMPTY MATRIX y FOR MODEL OUTPUT ##
   y              <- matrix(0,NDAYS,NOUT)
