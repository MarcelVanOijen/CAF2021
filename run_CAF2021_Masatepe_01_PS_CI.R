## run_CAF2021_Masatepe_01_PS_CI.R ##
## MvO, 2021-06-04

## INITIALISE SITE
   source('initialisation/initialise_CAF2021_Masatepe_01_PS_CI.R')
   
## Yield gap analysis (REMOVE later)
   params <- set_par( c("RAINMULT","NFERTMULT","SINKPMAX"),
                      c( 1        , 1         , 100 ) )
   
## RUN MODEL
   output <- run_model()

## Temporary extra printed output
   nd   <- dim(output)[1]
   iEnv <- c(4:24,55) ; iCoffee <- 25:38 ; iTrees <- 39:54
   print(output[ nd, ])

## OUTPUT ##
   # plot_output( vars=outputNames[ iEnv    ] )
   plot_output( vars=outputNames[ iCoffee ] )
   # plot_output( list_output=list(output), vars=c( "fTran(1)" ) )
   # plot_output( list_output=list(output), vars=c( "fNgrowth(1)" ) )
   # plot_output( list_output=list(output), vars=c( "CR_f" ) )
   # plot_output( list_output=list(output), vars=c( "CW_f" ) )
   # plot_output( list_output=list(output), vars=c( "CL_f" ) )
   # plot_output( list_output=list(output), vars=c( "CP_f" ) )
   