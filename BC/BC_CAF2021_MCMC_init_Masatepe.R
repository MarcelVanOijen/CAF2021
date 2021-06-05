## BC_CAF2021_MCMC_init_Masatepe.R
## MvO, 2021-05-20

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior <- "parameters/parameters_BC_Masatepe.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')

## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- c(
     "initialisation/initialise_CAF2021_Masatepe_01_PS_CI.R",
     "initialisation/initialise_CAF2021_Masatepe_02_PS_CM.R",
     "initialisation/initialise_CAF2021_Masatepe_03_SSTR_CM.R",
     "initialisation/initialise_CAF2021_Masatepe_04_SSTR_OI.R",
     "initialisation/initialise_CAF2021_Masatepe_05_SGTR_CI.R",
     "initialisation/initialise_CAF2021_Masatepe_06_SGTR_CM.R",
     "initialisation/initialise_CAF2021_Masatepe_07_SGTR_OI.R",
     "initialisation/initialise_CAF2021_Masatepe_08_SGTR_OM.R",
     "initialisation/initialise_CAF2021_Masatepe_09_ILSS_CI.R",
     "initialisation/initialise_CAF2021_Masatepe_10_ILSS_CM.R",
     "initialisation/initialise_CAF2021_Masatepe_11_ILSS_OI.R",
     "initialisation/initialise_CAF2021_Masatepe_12_ILSS_OM.R",
     "initialisation/initialise_CAF2021_Masatepe_13_ILSG_CM.R",
     "initialisation/initialise_CAF2021_Masatepe_14_ILSG_OI.R" )
   sitedata_filenames <- c(
     "data/data_Masatepe_01_PS_CI.txt",
     "data/data_Masatepe_02_PS_CM.txt",
     "data/data_Masatepe_03_SSTR_CM.txt",
     "data/data_Masatepe_04_SSTR_OI.txt",
     "data/data_Masatepe_05_SGTR_CI.txt",
     "data/data_Masatepe_06_SGTR_CM.txt",
     "data/data_Masatepe_07_SGTR_OI.txt",
     "data/data_Masatepe_08_SGTR_OM.txt",
     "data/data_Masatepe_09_ILSS_CI.txt",
     "data/data_Masatepe_10_ILSS_CM.txt",
     "data/data_Masatepe_11_ILSS_OI.txt",
     "data/data_Masatepe_12_ILSS_OM.txt",
     "data/data_Masatepe_13_ILSG_CM.txt",
     "data/data_Masatepe_14_ILSG_OI.txt" )
   nSites                 <- length(sitedata_filenames)
   sitelist               <- list() ; length(sitelist) <- nSites

## PROPOSAL TUNING FACTOR
   fPropTuning   <- 0.2 # This factor is used to modify Gelman's suggested average
                        # step length (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_CAF2021_MCMC_init_general.R')
