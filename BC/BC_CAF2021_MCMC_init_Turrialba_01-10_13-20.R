## BC_CAF2021_MCMC_init_Turrialba_01-10_13-20.R
## MvO, 2021-03-19

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior <- "parameters/parameters_BC_Turrialba_01-10_13-20.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')

## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- c(
      "initialisation/initialise_CAF2021_Turrialba_01_E_AC.R",
      "initialisation/initialise_CAF2021_Turrialba_02_E_MC.R",
      "initialisation/initialise_CAF2021_Turrialba_03_E_MO.R",
      "initialisation/initialise_CAF2021_Turrialba_04_E_BO.R",
      "initialisation/initialise_CAF2021_Turrialba_05_T_AC.R",
      "initialisation/initialise_CAF2021_Turrialba_06_T_MC.R",
      "initialisation/initialise_CAF2021_Turrialba_07_T_MO.R",
      "initialisation/initialise_CAF2021_Turrialba_08_T_BO.R",
      "initialisation/initialise_CAF2021_Turrialba_09_C_MC.R",
      "initialisation/initialise_CAF2021_Turrialba_10_C_MO.R",
      "initialisation/initialise_CAF2021_Turrialba_13_TE_MC.R",
      "initialisation/initialise_CAF2021_Turrialba_14_TE_MO.R",
      "initialisation/initialise_CAF2021_Turrialba_15_CE_AC.R",
      "initialisation/initialise_CAF2021_Turrialba_16_CE_MC.R",
      "initialisation/initialise_CAF2021_Turrialba_17_CE_MO.R",
      "initialisation/initialise_CAF2021_Turrialba_18_CE_BO.R",
      "initialisation/initialise_CAF2021_Turrialba_19_PS_AC.R",
      "initialisation/initialise_CAF2021_Turrialba_20_PS_MC.R" )
   sitedata_filenames     <- c(
      "data/data_Turrialba_01_E_AC.txt",
      "data/data_Turrialba_02_E_MC.txt",
      "data/data_Turrialba_03_E_MO.txt",
      "data/data_Turrialba_04_E_BO.txt",
      "data/data_Turrialba_05_T_AC.txt",
      "data/data_Turrialba_06_T_MC.txt",
      "data/data_Turrialba_07_T_MO.txt",
      "data/data_Turrialba_08_T_BO.txt",
      "data/data_Turrialba_09_C_MC.txt",
      "data/data_Turrialba_10_C_MO.txt",
      "data/data_Turrialba_13_TE_MC.txt",
      "data/data_Turrialba_14_TE_MO.txt",
      "data/data_Turrialba_15_CE_AC.txt",
      "data/data_Turrialba_16_CE_MC.txt",
      "data/data_Turrialba_17_CE_MO.txt",
      "data/data_Turrialba_18_CE_BO.txt",
      "data/data_Turrialba_19_PS_AC.txt",
      "data/data_Turrialba_20_PS_MC.txt" )
   nSites                 <- length(sitedata_filenames)
   sitelist               <- list() ; length(sitelist) <- nSites

## PROPOSAL TUNING FACTOR
   fPropTuning   <- 0.2 # This factor is used to modify Gelman's suggested average
                        # step length (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_CAF2021_MCMC_init_general.R')
