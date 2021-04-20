source('initialisation/initialise_CAF2021_general.R')

iH.model    <- which( outputNames=="harvDM_f_hay" )
 
idoy1.model <- which(output[,2]>=2003 & output[,3]==1)

params_BC_MAP       <- scparMAP_BC  * sc
params_BC_MaxL      <- scparMaxL_BC * sc

AvYield           <- matrix( NA, nrow=nSites, ncol=4 )
colnames(AvYield) <- c( "Site", "Y.data", "Y.MAP", "Y.ML" )
AvYield[,1]       <- c(1:10,13:20)

for (s in 1:nSites) {
  iH.data.s       <- which( data_name[[s]]=="harvDM_f_hay" )
  AvYield[s,2]    <- mean( data_value[[s]][iH.data.s] )

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
  AvYield[s,3]    <- mean( outputMAP[idoy1.model,iH.model] )

# Calculate model output for the MaxL parameter vector
  params[ip_BC_s] <- params_BC_MaxL     [icol_pChain_s]
  outputMaxL      <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
	            	            calendar_prunT, calendar_thinT, NDAYS )
  AvYield[s,4]    <- mean( outputMaxL[idoy1.model,iH.model] )
}

par( mfrow=c(1,1), mar=c(5,5,2,2) )

plot( AvYield[,2], AvYield[,3], main="Av. yield coffee (kg DM ha-1)",
      xlab="Observed", ylab="Simulated",
      xlim=c(0,max(AvYield[,2:3])), ylim=c(0,max(AvYield[,2:3])), type="n" )
text( AvYield[,2], AvYield[,3], labels=1:18, cex=0.8 )
abline(0,1,lty=2)
lm.Y <- lm(AvYield[,3]~AvYield[,2])
r2   <- signif( summary(lm.Y)$r.squared, 2 )
abline( lm.Y$coefficients[1], lm.Y$coefficients[2], col="blue" )
legend( "bottomright",
        legend=c("y=x", paste("Regression\nr2= ",as.character(r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )

