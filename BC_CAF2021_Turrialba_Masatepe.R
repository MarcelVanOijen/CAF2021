## BC_CAF2021_Turrialba_Masatepe.R
## MvO, 2021-06-10

   rm(list = ls())

## 1. INITIALISE MCMC ##
   nChain <- as.integer(1e5)
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
   iT <- 1:18 ; iM <- 19:32
   
   # source('initialisation/initialise_CAF2021_general.R')
   y_TM <- fYieldsTM( parMatrix=matMAP, sfiles=sitesettings_filenames )
   y_TM.obs <- y_TM[,"Y.obs"]
   y_TM.MAP <- y_TM[,"Y.sim"]
   yTM.lm   <- lm( y_TM.obs     ~ y_TM.MAP     )
   yT.lm    <- lm( y_TM.obs[iT] ~ y_TM.MAP[iT] )
   yM.lm    <- lm( y_TM.obs[iM] ~ y_TM.MAP[iM] )
   r2_condsTM_weatherTM <- summary(yTM.lm)$r.squared
   r2_condsT_weatherT   <- summary(yT.lm)$r.squared
   r2_condsM_weatherM   <- summary(yM.lm)$r.squared

   par( mfrow=c(1,3), mar=c(2,2,2,2), pty="s" )
   
   y_T <- y_TM[iT,]    ; y_M <- y_TM[iM,]
   plotYieldsTM( y_T ) ; plotYieldsTM( y_M ) ; plotYieldsTM( y_TM )

   fOutTM <- fOutputTM( parMatrix=matMAP, sfiles=sitesettings_filenames )
   
   par( mfrow=c(2,3), mar=c(3,2,2,0) )
   
   barplotTM( fOutTM[,"harvDM_f_hay"], name="Coffee (kg DM ha-1 y-1)" )
   barplotTM( fOutTM[,"harvCPT_2"   ], name="Fruit[2] (kg DM ha-1 y-1)" )
   barplotTM( fOutTM[,"harvCST_3"   ], name="Timber[3] (kg DM ha-1 y-1)" )
   
   barplotTM( fOutTM[,"D_Csys" ], name="System C (kg ha-1 y-1)" )
   barplotTM( fOutTM[,"D_Csoil"], name="Soil C (kg ha-1 y-1)"   )
   barplotTM( fOutTM[,"D_Nsoil"], name="Soil N (kg ha-1 y-1)", xtxt=1:32 )
   