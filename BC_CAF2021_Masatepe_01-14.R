## BC_CAF2021_Masatepe_01-14.R
## MvO, 2021-05-20

## 1. INITIALISE MCMC ##
   nChain <- as.integer(2e2)
   # nChain <- as.integer(1e5)
   source('BC/BC_CAF2021_MCMC_init_Masatepe_01-14.R')

## 2. RUNNING THE MCMC ##
   source('BC/BC_CAF2021_MCMC.R')

## 3. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')

## 4. SAVING WORKSPACE
   Rfile <- paste0('BC_',format(Sys.time(),"%H_%M.RData"))
   save.image( Rfile )
   