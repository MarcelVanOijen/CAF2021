## BC_CAF2021_MCMC_init_Turrialba_01-15-19.R
## MvO, 2021-11-24

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior <- "parameters/parameters_BC_Turrialba_01-15-19.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')

## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- c(
      "initialisation/initialise_CAF2021_Turrialba_01_E_AC.R",
      "initialisation/initialise_CAF2021_Turrialba_15_CE_AC.R",
      "initialisation/initialise_CAF2021_Turrialba_19_PS_AC.R" )
   sitedata_filenames     <- c(
      "data/data_Turrialba_01_E_AC.txt",
      "data/data_Turrialba_15_CE_AC.txt",
      "data/data_Turrialba_19_PS_AC.txt" )
   nSites                 <- length(sitedata_filenames)
   sitelist               <- list() ; length(sitelist) <- nSites

## PROPOSAL TUNING FACTOR
   fPropTuning   <- 0.2 # This factor is used to modify Gelman's suggested average
                        # step length (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_CAF2021_MCMC_init_general.R')
