## run_CAF2021_Masatepe_14_ILSG_OI.R ##
## MvO, 2021-05-20

## INITIALISE SITE
   source('initialisation/initialise_CAF2021_Masatepe_14_ILSG_OI.R')

## RUN MODEL
   output <- run_model()

## Temporary extra printed output
   nd   <- dim(output)[1]
   iEnv <- c(4:24,55) ; iCoffee <- 25:38 ; iTrees <- 39:54
   print(output[ nd, ])

## OUTPUT ##
   plot_output( vars=outputNames[ iEnv    ] )
   plot_output( vars=outputNames[ iCoffee ] )
   plot_output( vars=outputNames[ iTrees  ] )
   plot_output( list_output=list(output), vars=c( "fTranT_t(3)" ) )
   plot_output( list_output=list(output), vars=c( "D_Csoil_f_hay" ) )
   plot_output( list_output=list(output), vars=c( "D_Csys_f_hay" ) )
   plot_output( list_output=list(output), vars=c( "D_Nsoil_f_hay" ) )
   plot_output( list_output=list(output), vars=c( "NfixT_f_hay" ) )
   plot_output( list_output=list(output), vars=c( "Nleaching_f_hay" ) )
   plot_output( list_output=list(output), vars=c( paste0("Ac(",1:6,")"), "Shade_f" ) )
   plot_output( list_output=list(output), vars=paste0("z(",1:6,")") )
   
   iCST_t       <- which( startsWith(outputNames ,"CST_t("     ) )
   itreedens_t  <- which( startsWith(outputNames ,"treedens_t(") )
   ih_t         <- which( startsWith(outputNames ,"h_t("       ) )
   iKH          <- which( startsWith(names_params,"KH("        ) )
   iKHEXP       <- which( startsWith(names_params,"KHEXP("     ) )
   h_t1         <- output[,ih_t[1]]  ; h_t3   <- output[,ih_t[3]]
   CSpertree_t1 <- output[,iCST_t[1]] / output[,itreedens_t[1]]
   CSpertree_t3 <- output[,iCST_t[3]] / output[,itreedens_t[3]]
   KH1          <- params[iKH[1]]    ; KH3    <- params[iKH[3]]
   KHEXP1       <- params[iKHEXP[1]] ; KHEXP3 <- params[iKHEXP[3]]
   
   Time         <- output[,1]
   iTimefun     <- seq(1,NDAYS,100)
   Timefun      <- Time[iTimefun]
   h_t1fun      <- KH1 * CSpertree_t1[iTimefun] ^ KHEXP1
   h_t3fun      <- KH3 * CSpertree_t3[iTimefun] ^ KHEXP3
   
   par( mfrow=c(1,2) )
   
   ymax <- max(CSpertree_t1, CSpertree_t3)
   plot( Time, CSpertree_t1, ylim=c(0,ymax), type="l" )
   points( Time, CSpertree_t3, type="l", col="red" )
   legend( "topleft", c("CSpertree(1)","CSpertree(3)"),
           lty=1, col=c("black","red") )
   
   ymax <- max(h_t1, h_t3, h_t1fun, h_t3fun)
   plot( Time, h_t1, ylim=c(0,ymax), type="l" )
   points( Timefun, h_t1fun )
   points( Time, h_t3, ylim=c(0,ymax), type="l", col="red" )
   points( Timefun, h_t3fun, col="red" )
   legend( "topleft", c("h(1)","h(3)"),
           lty=1, col=c("black","red") )

   tail(output,1)
   # 2.611017 186.7073 0.9235559
   # 2.635528 186.7674 0.9235564 (calculation of land-cover areas now at the top)