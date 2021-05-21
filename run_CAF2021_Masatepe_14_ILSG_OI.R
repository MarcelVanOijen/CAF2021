## run_CAF2021_Masatepe_14_ILSG_OI.R ##
## MvO, 2021-05-20

## INITIALISE SITE
   source('initialisation/initialise_CAF2021_Masatepe_14_ILSG_OI.R')

## RUN MODEL
   output <- run_model()

## Temporary extra printed output
   nd   <- dim(output)[1]
   iEnv <- c(4:24,55) ; iCoffee <- 25:38 ; iTrees <- 39:54
   print(output[ nd,    ])

## OUTPUT ##
   plot_output( vars=outputNames[ iEnv    ] )
   plot_output( vars=outputNames[ iCoffee ] )
   plot_output( vars=outputNames[ iTrees  ] )
   plot_output( list_output=list(output), vars=c( "fTranT_t(3)" ) )
   
