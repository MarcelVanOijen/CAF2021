## BC_CAF2021_G.R
## MvO, 2021-08-24

   # rm(list = ls())

## 1. INITIALISE MCMC ##
   nChain <- as.integer(2e1)
   source('BC/BC_CAF2021_MCMC_init_G.R')

## 2. RUNNING THE MCMC ##
   source('BC/BC_CAF2021_MCMC.R')

## 3. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')

## 4. SAVING WORKSPACE
   Rfile <- paste0('BC_G_',format(Sys.time(),"%H_%M.RData"))
   save.image( Rfile )
   