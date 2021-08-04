## run_CAF2021_G_fi.R ##
## MvO, 2021-08-03

## INITIALISE SITE
   fi      <- 82
   C_G     <- df_G.f$C.soil [ fi ]
   CN_G    <- df_G.f$CN.soil[ fi ]
   LAT_G   <- df_G.f$lat    [ fi ]
   SLOPE_G <- df_G.f$slope  [ fi ]
   Nfert_G <- df_G.f$Nfert  [ fi ]
   source( sitesettings_filenames[1] )
   matrix_weather_fi <- read_listweather_CAF( df_G.f$weather.WC, fi )
      
   calendar_fert_fi <- matrix( -1, nrow=100, ncol=3 )
   calendar_fert_fi [ 1:48, 1 ] <- rep( 2002:2017, each=3 )
   calendar_fert_fi [ 1:48, 2 ] <- c( 135, 289, 350 )
   calendar_fert_fi [ 1:48, 3 ] <- rep( Nfert_G/3, 3 )
      
   if(df_G.f$nt[fi]==0)   source(sitesettings_filenames[19])
   if(df_G.f$nt[fi]==1) { source(sitesettings_filenames[31])
     params<-set_par("TREEDENS0(3)",0) }
   if(df_G.f$nt[fi]==2) { source(sitesettings_filenames[31]) } 
   if(df_G.f$nt[fi] >2) { source(sitesettings_filenames[31])
     params<-set_par_speciesT(2,"Banana")
   }
   params[ c(ip_CNLITT0,ip_CNSOMF0,ip_CNSOMS0) ] <- CN_G
   params[ ip_CSOM0                            ] <- C_G
   params[ ip_LAT                              ] <- LAT_G
   params[ ip_SLOPE                            ] <- SLOPE_G
   
## RUN MODEL

   # params <- set_par("TPLUS",-3)
   
   output <- run_model( params, matrix_weather_fi, calendar_fert_fi,
                        calendar_prunC, calendar_prunT, calendar_thinT,
                        NDAYS )
   # outputSummary( output )

## Temporary extra printed output
   nd    <- dim(output)[1]
   iEnv  <- c(4:24,55) ; iCoffee <- 25:38 ; iTrees <- 39:54
   iPhen <- c(122,127,125,130) # DVS(1), DVS(2), DayFl(1), DayFl(2)
   print( output[ nd, ] )

## OUTPUT ##
   plot_output( vars=outputNames[ iEnv    ] )
   plot_output( vars=outputNames[ iCoffee ] )
   plot_output( vars=outputNames[ iTrees  ] )
   plot_output( vars=outputNames[ iPhen  ] )

   par( mfrow=c(3,1) )
   days <- 2000:3000

   plot  ( output[days,4], type="l", ylim=c(0,0.5) )
   points( output[days,5], type="l", col="red" )
   points( output[days,6], type="l", col="blue" )
   points( output[days,7], type="l", col="green" )
   points( output[days,8], type="l", col="yellow" )
   points( output[days,9], type="l", col="brown" )
   
   plot  ( output[days,122], type="l", ylim=c(0,2) )
   points( output[days,127], type="l", col="red" )
   points( 1+output[days,3]/365, type="l", col="blue", lty=4 )
   points( output[days,130], type="l", col="green", lty=4 )
   
   plot  ( output[days,26], type="l" )
   points( output[days,27], type="l", col="red" )
   points( output[days,28], type="l", col="blue" )
   points( output[days,29], type="l", col="green" )
   points( output[days,30], type="l", col="yellow" )
   points( output[days,31], type="l", col="brown" )
   points( output[days,32]/1e5, type="l", col="blue", lty=4 )
    
   par( mfrow=c(1,1) )
   plot( output[days,32]/1e5, type="l", col="blue", lty=4 )
   
   daysFl    <- which( output[,125] == 1 )
   doyFl     <- output[daysFl,3]    ; doyFl
   daysHarv1 <- which( output[,164] == 1 )
   daysHarv2 <- which( output[,165] == 1 )
   daysHarv3 <- which( output[,166] == 1 )
   daysHarv4 <- which( output[,167] == 1 )
   daysHarv5 <- which( output[,168] == 1 )
   daysHarv6 <- which( output[,169] == 1 )
   doyHarv1  <- output[daysHarv1,3] ; doyHarv1
   doyHarv2  <- output[daysHarv2,3] ; doyHarv2
   doyHarv3  <- output[daysHarv3,3] ; doyHarv3
   doyHarv4  <- output[daysHarv4,3] ; doyHarv4
   doyHarv5  <- output[daysHarv5,3] ; doyHarv5
   doyHarv6  <- output[daysHarv6,3] ; doyHarv6
   