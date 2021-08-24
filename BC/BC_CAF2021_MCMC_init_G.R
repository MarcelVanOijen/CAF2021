## BC_CAF2021_MCMC_init_G.R
## MvO, 2021-08-24

  load( "df_G.f_final.rda" ) ; n_G.f <- dim(df_G.f)[1]

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior <- "parameters/parameters_BC_G.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')

## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- paste0(
      "initialisation/CG/inititialise_CAF2021_G_f", 1:n_G.f, ".R" )
   sitedata_filenames     <- paste0(
      "data/CG/data_G_f", 1:n_G.f, ".txt" )
   nSites                 <- length(sitedata_filenames)
   sitelist               <- list() ; length(sitelist) <- nSites

## PROPOSAL TUNING FACTOR
   fPropTuning   <- 0.2 # This factor is used to modify Gelman's suggested average
                        # step length (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_CAF2021_MCMC_init_general.R')
