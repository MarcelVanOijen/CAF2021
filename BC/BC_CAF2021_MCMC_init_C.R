## BC_CAF2021_MCMC_init_C.R
## MvO, 2021-08-23

  load( "df_C.f_final.rda" ) ; n_C.f <- dim(df_C.f)[1]

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior <- "parameters/parameters_BC_CG.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')

## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- paste0(
      "initialisation/CG/inititialise_CAF2021_C_f", 1:n_C.f, ".R" )
   sitedata_filenames     <- paste0(
      "data/CG/data_C_f", 1:n_C.f, ".txt" )
   nSites                 <- length(sitedata_filenames)
   sitelist               <- list() ; length(sitelist) <- nSites

## PROPOSAL TUNING FACTOR
   fPropTuning   <- 0.2 # This factor is used to modify Gelman's suggested average
                        # step length (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_CAF2021_MCMC_init_general.R')
