source('initialisation/initialise_CAF2021_general.R')

iyear.model       <- which( outputNames=="year" )
idoy.model        <- which( outputNames=="doy" )
iH.model          <- which( outputNames=="harvDM_f_hay" )
iNfert.model      <- which( outputNames=="Nfert_f" )

params_BC_MAP     <- scparMAP_BC  * sc

Yields             <- vector( "list", nSites )

AvYield           <- matrix( NA, nrow=nSites, ncol=4 )
colnames(AvYield) <- c( "Site", "Y.data", "Y.MAP", "Nfert" )
AvYield[,1]       <- 1:nSites

for (s in 1:nSites) {
  iH.data.s       <- which( data_name[[s]]=="harvDM_f_hay" )
  nY.s            <- length( iH.data.s )
  
  year.data.s     <- data_year [[s]] [iH.data.s]
  doy.data.s      <- data_doy  [[s]] [iH.data.s]
  H.data.s        <- data_value[[s]] [iH.data.s]
  
  Yields[[s]]           <- matrix( NA, nrow=nY.s, ncol=6 )
  colnames(Yields[[s]]) <- c( "Site", "year", "doy", "Y.data", "Y.MAP", "Nfert" )
  Yields[[s]][,1  ]     <- s
  Yields[[s]][,2:4]     <- cbind( data_year [[s]], data_doy[[s]],
                                  data_value[[s]] ) [iH.data.s,]
  
  AvYield[s,2]    <- mean( Yields[[s]][,"Y.data"] )

  params          <- list_params        [[s]]
  matrix_weather  <- list_matrix_weather[[s]]
  calendar_fert   <- list_calendar_fert [[s]] ; calendar_prunC <- list_calendar_prunC[[s]] 
  calendar_prunT  <- list_calendar_prunT[[s]] ; calendar_thinT <- list_calendar_thinT[[s]]  
  NDAYS           <- list_NDAYS         [[s]]       
  ip_BC_s         <- ip_BC_site         [[s]]
  icol_pChain_s   <- icol_pChain_site   [[s]]

# Calculate model output for the MAP parameter vector
  params[ip_BC_s] <- params_BC_MAP      [icol_pChain_s]

  # Parameter change:  
  params          <- set_par( set_parNames, set_parValues )
  
  outputMAP       <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                                calendar_prunT, calendar_thinT, NDAYS )
  
  iH.model.s      <- sapply( 1:nY.s, function(i) {
    which( outputMAP[,iyear.model]==year.data.s[i] & 
           outputMAP[,idoy.model ]==doy.data.s [i] ) } )
  Yields[[s]][,"Y.MAP"] <- round( outputMAP[iH.model.s,iH.model], 0 )
  AvYield[s   ,"Y.MAP"] <- mean ( outputMAP[iH.model.s,iH.model] )
  Yields[[s]][,"Nfert"] <- mean ( outputMAP[,iNfert.model] ) * 365 * 1e4
  AvYield[s   ,"Nfert"] <- mean ( outputMAP[,iNfert.model] ) * 365 * 1e4
}
