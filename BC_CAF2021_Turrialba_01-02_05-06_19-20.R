## BC_CAF2021_Turrialba_01-02_05-06_19-20.R
## MvO, 2021-03-19

## 1. INITIALISE MCMC ##
   nChain <- as.integer(100)
   source('BC/BC_CAF2021_MCMC_init_Turrialba_01-02_05-06_19-20.R')

## 2. RUNNING THE MCMC ##
   source('BC/BC_CAF2021_MCMC.R')

## 3. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')
