## run_CAF2021_Turrialba_20_PS_MC.R ##
## MvO, 2021-03-19

## INITIALISE SITE
   source('initialisation/initialise_CAF2021_Turrialba_20_PS_MC.R')

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
