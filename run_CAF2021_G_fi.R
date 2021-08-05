## run_CAF2021_G_fi.R ##
## MvO, 2021-08-04

################################################################################
  # load( "df_C.f.rda" )
  # load( "df_G.f.rda" )
  # 
  # filesTM <- file.info(list.files(pattern="^BC_Turrialba_Masatepe.*\\.RData$"))
  # filesTM <- filesTM[ with(filesTM,order(as.POSIXct(mtime))), ]
  # fileTM  <- tail( rownames(filesTM[1]), 1 )
  # load( fileTM )
  # 
  # filesTM_MAP <- file.info(list.files(pattern="^CAF2021_parMAP.*\\.rds$"))
  # filesTM_MAP <- filesTM_MAP[ with(filesTM_MAP,order(as.POSIXct(mtime))), ]
  # fileTM_MAP  <- tail( rownames(filesTM_MAP[1]), 1 )
  # # parMAP      <- readRDS( fileTM_MAP )
  # parMAP.old  <- readRDS( fileTM_MAP ) ; np.old <- dim(parMAP.old)[1]
  
  # Construct new parMAP (with more parameters)
  source('initialisation/set_CAF2021_Turrialba.R')
  np               <- length( params )
  parMAP           <- matrix( params, nrow=np, ncol=nSites, byrow=F )
  rownames(parMAP) <- names_params
  colnames(parMAP) <- paste0( "s", 1:nSites )
  for(p in 1:np.old) {
    prow          <- which( startsWith( rownames(parMAP),
                                        rownames(parMAP.old)[p] ) )
    for(r in prow) { parMAP[r,] <- parMAP.old[p,] }
  }
  
################################################################################
  
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
      
   if(df_G.f$nt[fi]==0) { source(sitesettings_filenames[19])
     params <- parMAP[,19]
   }
   if(df_G.f$nt[fi]==1) { source(sitesettings_filenames[31])
     params <- parMAP[,31]
     params <- set_par("TREEDENS0(3)",0)
   }
   if(df_G.f$nt[fi]==2) { source(sitesettings_filenames[31])
     params <- parMAP[,31]
   } 
   if(df_G.f$nt[fi] >2) { source(sitesettings_filenames[31])
     params <- parMAP[,31]
     params <- set_par_speciesT(2,"Banana")
   }
   ip_CNLITT0  <- which( names_params=="CNLITT0" )
   ip_CNSOMF0  <- which( names_params=="CNSOMF0" )
   ip_CNSOMS0  <- which( names_params=="CNSOMS0" )
   ip_CSOM0    <- which( names_params=="CSOM0"   )
   ip_LAT      <- which( names_params=="LAT"     )
   ip_SLOPE    <- which( names_params=="SLOPE"   )
   params[ c(ip_CNLITT0,ip_CNSOMF0,ip_CNSOMS0) ] <- CN_G
   params[ ip_CSOM0                            ] <- C_G
   params[ ip_LAT                              ] <- LAT_G
   params[ ip_SLOPE                            ] <- SLOPE_G
   
## RUN MODEL

   params <- set_par("TPLUS",1.5)
   params <- set_par("SHADETARGET",-1)
   
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
   
   plot  ( output[days,123], type="l", ylim=c(0,2) )
   points( output[days,127], type="l", col="red" )
   points( 1+output[days,3]/365, type="l", col="blue", lty=4 )
   points( output[days,122], type="l", col="green", lty=4 )
   
   plot  ( output[days,26], type="l" )
   points( output[days,27], type="l", col="red" )
   points( output[days,28], type="l", col="blue" )
   points( output[days,29], type="l", col="green" )
   points( output[days,30], type="l", col="yellow" )
   points( output[days,31], type="l", col="brown" )
   points( output[days,32]/1e5, type="l", col="blue", lty=4 )
    
   daysFl    <- which( output[,122] == 1 )
   doyFl     <- output[daysFl,3]    ; doyFl
   # 152 152 148 152 152 152 152 152 133 152 152 152
   
   daysHarv1 <- which( output[,163] == 1 )
   daysHarv2 <- which( output[,164] == 1 )
   daysHarv3 <- which( output[,165] == 1 )
   daysHarv4 <- which( output[,166] == 1 )
   daysHarv5 <- which( output[,167] == 1 )
   daysHarv6 <- which( output[,168] == 1 )
   doyHarv1  <- output[daysHarv1,3] ; doyHarv1
   doyHarv2  <- output[daysHarv2,3] ; doyHarv2
   doyHarv3  <- output[daysHarv3,3] ; doyHarv3
   doyHarv4  <- output[daysHarv4,3] ; doyHarv4
   doyHarv5  <- output[daysHarv5,3] ; doyHarv5
   doyHarv6  <- output[daysHarv6,3] ; doyHarv6
   # 126 129 120 131 131 131 137 124 116 126 128
   # 152 148 152 152 152 152 152 133 152 152 152
   # 152 148 152 152 152 152 152 133 152 152 152
   # 152 148 152 152 152 152 152 133 150 153
   # 126 129 120 131 131 131 137 124 116 126 128
   # 126 129 120 131 131 131 137 124 116 126 128
   
   d181 <- which( output[,which(outputNames=="doy")] == 181 )
   mean( output[ d181, which(outputNames=="harvDM_f_hay") ] )
   # 1554.538
   