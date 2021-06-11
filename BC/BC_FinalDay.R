source('initialisation/initialise_CAF2021_general.R')

params_BC_MAP            <- scparMAP_BC * sc

outputFinalDay           <- matrix( NA, nrow=nSites, ncol=NOUT )
colnames(outputFinalDay) <- outputNames

for (s in 1:nSites) {
  params          <- list_params        [[s]] ; matrix_weather <- list_matrix_weather[[s]]
  calendar_fert   <- list_calendar_fert [[s]] ; calendar_prunC <- list_calendar_prunC[[s]] 
  calendar_prunT  <- list_calendar_prunT[[s]] ; calendar_thinT <- list_calendar_thinT[[s]]  
  NDAYS           <- list_NDAYS         [[s]]       
  ip_BC_s         <- ip_BC_site         [[s]]
  icol_pChain_s   <- icol_pChain_site   [[s]]
  
  # Calculate model output for the MAP parameter vector
  params[ip_BC_s] <- params_BC_MAP      [icol_pChain_s]
  outputMAP       <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                                calendar_prunT, calendar_thinT, NDAYS )
  outputFinalDay[s,] <- outputMAP[ NDAYS, ]
}
