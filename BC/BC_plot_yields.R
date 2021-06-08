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
  Yields[[s]][,"Nfert"] <- mean( outputMAP[,iNfert.model] ) * 365 * 1e4
  AvYield[s,"Nfert"]    <- mean( outputMAP[,iNfert.model] ) * 365 * 1e4
}

par( mfrow=c(1,1), mar=c(5,5,2,2) )

AllYields      <- do.call(rbind, Yields)
AllYields.data <- AllYields[,"Y.data"] ; AllYields.model <- AllYields[,"Y.MAP"]
plot( AllYields.model, AllYields.data, main="Yields coffee\n(kg DM ha-1 y-1)",
      xlab="Sim.", ylab="Obs.",
      cex.main=1, type="n" )
colVector <- rep("",nSites)
colVector[ which( AllYields[,"Site"]<=18 ) ] <- "black"
colVector[ which( AllYields[,"Site"]>=19 ) ] <- "red"
text( AllYields.model, AllYields.data,
      labels=AllYields[,"Site"], cex=0.8, col=colVector )
abline(0,1,lty=2)
AllYields.lm <- lm(AllYields[,"Y.data"]~AllYields[,"Y.MAP"])
AllYields.r2 <- signif( summary(AllYields.lm)$r.squared, 2 )
abline( AllYields.lm$coefficients[1], AllYields.lm$coefficients[2], col="blue" )
legend( "topleft",
        legend=c("y=x", paste("r2= ",as.character(AllYields.r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )


par( mfrow=c(1,2), mar=c(5,2,2,1) )

s.plot <- 1:nSites # Sites to plot
# s.plot <-  1:18 # Sites to plot
# s.plot <- 19:32 # Sites to plot

plot( AvYield[s.plot,"Nfert"], AvYield[s.plot,"Y.data"],
      main="Av. yield coffee\n(kg DM ha-1 y-1)",
      xlab="N fert. (kg N ha-1 y-1)", ylab="Obs.",
      ylim=c(0,AvYields.max),
      cex.main=1, type="n" )
colVector <- rep("",length(s.plot))
colVector[ which( AvYield[s.plot,"Site"]<=18 ) ] <- "black"
colVector[ which( AvYield[s.plot,"Site"]>=19 ) ] <- "red"
text( AvYield[s.plot,"Nfert"], AvYield[s.plot,"Y.data"],
      labels=AvYield[s.plot,"Site"], cex=0.8, col=colVector )
AvYield.Nfert.lm <- lm(AvYield[s.plot,"Y.data"]~AvYield[s.plot,"Nfert"])
r2   <- signif( summary(AvYield.Nfert.lm)$r.squared, 2 )
abline( AvYield.Nfert.lm$coefficients[1], AvYield.Nfert.lm$coefficients[2],
        col="blue" )
legend( "bottomright",
        legend=c("y=x", paste("r2= ",as.character(r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )

AvYields.max   <- max(AvYield[s.plot,c("Y.data","Y.MAP")])
plot( AvYield[s.plot,"Y.MAP"], AvYield[s.plot,"Y.data"],
      main="Av. yield coffee\n(kg DM ha-1 y-1)",
      xlab="Sim. yield (kg ha-1)", ylab="",
      xlim=c(0,AvYields.max), ylim=c(0,AvYields.max),
      cex.main=1, type="n" )
colVector <- rep("",length(s.plot))
colVector[ which( AvYield[s.plot,"Site"]<=18 ) ] <- "black"
colVector[ which( AvYield[s.plot,"Site"]>=19 ) ] <- "red"
text( AvYield[s.plot,"Y.MAP"], AvYield[s.plot,"Y.data"],
      labels=AvYield[s.plot,"Site"], cex=0.8, col=colVector )
abline(0,1,lty=2)
AvYield.lm <- lm(AvYield[s.plot,"Y.data"]~AvYield[s.plot,"Y.MAP"])
r2   <- signif( summary(AvYield.lm)$r.squared, 2 )
abline( AvYield.lm$coefficients[1], AvYield.lm$coefficients[2],
        col="blue" )
legend( "bottomright",
        legend=c("y=x", paste("r2= ",as.character(r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )

