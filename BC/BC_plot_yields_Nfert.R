source('initialisation/initialise_CAF2021_general.R')

iyear.model <- which( outputNames=="year" )
idoy.model  <- which( outputNames=="doy" )
iH.model    <- which( outputNames=="harvDM_f_hay" )

params_BC_MAP     <- scparMAP_BC  * sc
params_BC_MaxL    <- scparMaxL_BC * sc

Yields             <- vector( "list", nSites )

AvYield           <- matrix( NA, nrow=nSites, ncol=4 )
colnames(AvYield) <- c( "Site", "Y.data", "Y.MAP", "Y.ML" )
AvYield[,1]       <- 1:nSites

for (s in 1:nSites) {
  iH.data.s       <- which( data_name[[s]]=="harvDM_f_hay" )
  nY.s            <- length( iH.data.s )
  
  year.data.s     <- data_year [[s]] [iH.data.s]
  doy.data.s      <- data_doy  [[s]] [iH.data.s]
  H.data.s        <- data_value[[s]] [iH.data.s]
  
  Yields[[s]]           <- matrix( NA, nrow=nY.s, ncol=5 )
  colnames(Yields[[s]]) <- c( "Site", "year", "doy", "Y.data", "Y.MAP" )
  Yields[[s]][,1  ]     <- s
  Yields[[s]][,2:4]     <- cbind( data_year[[s]], data_doy[[s]],
                                  data_value[[s]] ) [iH.data.s,]
  
  AvYield[s,2]    <- mean( Yields[[s]][,"Y.data"] )

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
  
  iH.model.s      <- sapply( 1:nY.s, function(i) {
    which( outputMAP[,iyear.model]==year.data.s[i] & 
           outputMAP[,idoy.model ]==doy.data.s [i] ) } )
  Yields[[s]][,"Y.MAP"] <- round( outputMAP[iH.model.s,iH.model], 0 )
  AvYield[s,"Y.MAP"]    <- mean ( outputMAP[iH.model.s,iH.model] )
  
# Calculate model output for the MaxL parameter vector
  params[ip_BC_s]   <- params_BC_MaxL[ icol_pChain_s ]
  outputMaxL        <- run_model( params, matrix_weather,
                                  calendar_fert, calendar_prunC,
	            	                  calendar_prunT, calendar_thinT, NDAYS )
  AvYield[s,"Y.ML"] <- mean( outputMaxL[iH.model.s,iH.model] )
}

par( mfrow=c(1,2), mar=c(5,5,2,2) )

AllYields      <- do.call(rbind, Yields)
AllYields.data <- AllYields[,"Y.data"] ; AllYields.model <- AllYields[,"Y.MAP"]
Yields.max     <- max( AllYields.data, AllYields.model )
plot( AllYields.data, AllYields.model, main="Yields coffee\n(kg DM ha-1)",
      xlab="Obs.", ylab="Sim.",
      xlim=c(0,Yields.max), ylim=c(0,Yields.max),
      cex.main=1, type="n" )
text( AllYields.data, AllYields.model,
      labels=AllYields[,1], cex=0.8 )
abline(0,1,lty=2)
AllYields.lm <- lm(AllYields[,"Y.MAP"]~AllYields[,"Y.data"])
AllYields.r2 <- signif( summary(AllYields.lm)$r.squared, 2 )
abline( AllYields.lm$coefficients[1], AllYields.lm$coefficients[2], col="blue" )
legend( "topleft",
        legend=c("y=x", paste("r2= ",as.character(AllYields.r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )


AvYields.max   <- max(AvYield[,c("Y.data","Y.MAP")])
plot( AvYield[,"Y.data"], AvYield[,"Y.MAP"],
      main="Av. yield coffee\n(kg DM ha-1)",
      xlab="Obs.", ylab="Sim.",
      xlim=c(0,AvYields.max), ylim=c(0,AvYields.max),
      cex.main=1, type="n" )
text( AvYield[,"Y.data"], AvYield[,"Y.MAP"],
      labels=AvYield[,1], cex=0.8 )
abline(0,1,lty=2)
lm.Y <- lm(AvYield[,"Y.MAP"]~AvYield[,"Y.data"])
r2   <- signif( summary(lm.Y)$r.squared, 2 )
abline( lm.Y$coefficients[1], lm.Y$coefficients[2], col="blue" )
legend( "bottomright",
        legend=c("y=x", paste("r2= ",as.character(r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )

