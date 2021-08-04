## run_CAF2021_Turrialba_01_E_AC.R ##
## MvO, 2021-03-19

## INITIALISE SITE
   source('initialisation/initialise_CAF2021_Turrialba_01_E_AC.R')

## RUN MODEL
   output <- run_model()

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

   par( mfrow=c(2,1) )
   days <- 500:1000
   plot  ( output[days,122] ) ; points( output[days,127], col="red" )
   plot  ( output[days,125] ) ; points( output[days,130], col="red" )
   
   daysFl12  <- which( output[,125] == 1 )
   doyFl12   <- output[daysFl12,3]
   daysHarv1 <- which( output[,122] >= 1 )
   daysHarv2 <- which( output[,127] >= 1 )
   doyHarv1  <- output[daysHarv1,3] ; doyHarv1
   # 216 183 210 170 186 210 251 205 208 182 206 224 251 188 194 218
   doyHarv2  <- output[daysHarv2,3] ; doyHarv2
   # 254 222 251 208 230 249 291 248 249 226 251 265 288 231 232 256
   