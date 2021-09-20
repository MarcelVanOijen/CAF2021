## BC_CAF2021_Turrialba_Masatepe.R
## MvO, 2021-09-16

   rm(list = ls())

## 1. INITIALISE MCMC ##
   nChain <- as.integer(6e4)
   # nChain <- as.integer(3e5)
   source('BC/BC_CAF2021_MCMC_init_Turrialba_Masatepe.R')

## 2. RUNNING THE MCMC ##
   source('BC/BC_CAF2021_MCMC.R')

## 3. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')

## 4. SAVING WORKSPACE
   Rfile <- paste0('BC_Turrialba_Masatepe_',format(Sys.time(),"%H_%M.RData"))
   save.image( Rfile )
   
## 5. Post-processing results from BC

   ## Start here when BC was carried out in an earlier session
   # load("BC_Turrialba_Masatepe_10_46.RData") ; np <- length( params )

   ## Optional restoring old parameterisation for comparison with new MAP results
   # matMAP.old <- readRDS( "BC/BC_RESULTS_Turrialba-Masatepe/CAF2021_parMAP_08_36.rds")
   # np.old     <- dim(matMAP_June26)[1]
   # for(p in 1:np.old) {
   #   prow <- which( startsWith( rownames(matMAP), rownames(matMAP.old)[p] ) )
   #   for(r in prow) { matMAP[r,] <- matMAP.old[p,] } }
   ## End restoring old parameterisation
   
   y_TM <- fYieldsTM( parMatrix=matMAP, sfiles=sitesettings_filenames )
   iT   <- 1:18      ; iM  <- 19:32
   y_T  <- y_TM[iT,] ; y_M <- y_TM[iM,]
   
   y_TM.obs <- y_TM[,"Y.obs"] ; y_TM.MAP <- y_TM[,"Y.sim"]
   yTM.lm   <- lm( y_TM.obs     ~ y_TM.MAP     )
   yT.lm    <- lm( y_TM.obs[iT] ~ y_TM.MAP[iT] )
   yM.lm    <- lm( y_TM.obs[iM] ~ y_TM.MAP[iM] )

   par( mfrow=c(1,3), mar=c(2,2,2,2), pty="s" )
   plotYieldsTM( y_T ) ; plotYieldsTM( y_M ) ; plotYieldsTM( y_TM )

   fOutTM <- fOutputTM( parMatrix=matMAP, sfiles=sitesettings_filenames )
   
   par( mfrow=c(2,3), mar=c(3,2,2,0) )
   barplotTM( fOutTM[,"harvDM_f_hay"], name="Coffee (kg DM ha-1 y-1)" )
   barplotTM( fOutTM[,"harvCPT_2"   ], name="Fruit[2] (kg DM ha-1 y-1)" )
   barplotTM( fOutTM[,"harvCST_3"   ], name="Timber[3] (kg DM ha-1 y-1)" )
   barplotTM( fOutTM[,"D_Csys"      ], name="System C (kg ha-1 y-1)" )
   barplotTM( fOutTM[,"D_Csoil"     ], name="Soil C (kg ha-1 y-1)"   )
   barplotTM( fOutTM[,"D_Nsoil"     ], name="Soil N (kg ha-1 y-1)", xtxt=1:32 )
   