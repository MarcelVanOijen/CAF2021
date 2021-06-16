plot_outputs_data_s_PNG <- function(
  isite        = 1,
  list_runs    = list_runs,
  nruns        = length(list_runs),
  leg_title    = "LEGEND",
  leg          = as.character(1:nruns),
  cols         = 1:nruns,
  lwds	       = rep(3,nruns),
  ltys         = rep(1,nruns) ) {
  
  s <- isite
  
  outputsMeasured      <- unique(data_index[[s]])
  noutputsMeasured   <- length(outputsMeasured)
  nrowsPlots           <- ceiling(sqrt(noutputsMeasured))
  ncolsPlots           <- ceiling((noutputsMeasured)/nrowsPlots)
  par(mfrow=c(nrowsPlots,ncolsPlots),omi=c(0,0,1,0), mar=c(2, 2, 3, 1) )
  
  for (p in outputsMeasured) {
    datap     <- which( data_name[[s]] == as.character(outputNames[p]) )
    lcl       <- data_value[[s]][datap] - data_sd[[s]][datap]
    ucl       <- data_value[[s]][datap] + data_sd[[s]][datap]
    g_range_p <- range( sapply( 1:nruns, function(i){range(list_runs[[i]][,p])} ) )
    g_range   <- range( g_range_p, lcl, ucl )
    plot( list_runs[[1]][,1], list_runs[[1]][,p], type='l',
          xlab="", ylab="", ylim=g_range, cex.main=2.5,
          main=paste(outputNames[p]," ",outputUnits[p],sep=""),
          col=cols[1], lwd=lwds[1], lty=ltys[1] )
    if (nruns>=2) {
      for (i in 2:nruns) {
        points( list_runs[[i]][,1], list_runs[[i]][,p], type='l',
                col=cols[i], lwd=lwds[i], lty=ltys[i] )
      }
    }
    points( data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, data_value[[s]][datap],
            col='blue', lwd=2, cex=2 )
    arrows( data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, ucl,
            data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, lcl,
            col='blue', lwd=2, angle=90, code=3, length=0.05 )
  }
  
  sitenames <- gsub( ".R", "", sub(".*CAF2021_","",sitesettings_filenames),
                     fixed=TRUE )
  mtext( paste("TREATMENT ",s," (",sitenames[s],")",sep=""),
         side=3, line=1, outer=TRUE, cex=3, font=2)
}

################################################################################

params_BC_ModePrior <-   parmod_BC
params_BC_MAP       <- scparMAP_BC  * sc
params_BC_MaxL      <- scparMaxL_BC * sc

for (s in 1:nSites) {
  params          <- list_params        [[s]] ; matrix_weather <- list_matrix_weather[[s]]
  calendar_fert   <- list_calendar_fert [[s]] ; calendar_prunC <- list_calendar_prunC[[s]] 
  calendar_prunT  <- list_calendar_prunT[[s]] ; calendar_thinT <- list_calendar_thinT[[s]]  
  NDAYS           <- list_NDAYS         [[s]]       
  ip_BC_s         <- ip_BC_site         [[s]]
  icol_pChain_s   <- icol_pChain_site   [[s]]
# Calculate model output for the prior mode
  run_model() # To avoid LAI(1)-problems for site 2 (which arose when site1=shaded, site2=fullsun)
  params[ip_BC_s] <- params_BC_ModePrior[icol_pChain_s]
  outputPriorMode <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                                calendar_prunT, calendar_thinT, NDAYS )

# Calculate model output for the MAP parameter vector
  params[ip_BC_s] <- params_BC_MAP      [icol_pChain_s]
  outputMAP       <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                                calendar_prunT, calendar_thinT, NDAYS )

# Calculate model output for the MaxL parameter vector
  params[ip_BC_s] <- params_BC_MaxL     [icol_pChain_s]
  outputMaxL      <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
	            	            calendar_prunT, calendar_thinT, NDAYS )
  
# # Calculate model output for a sample from the posterior
# # Take a sample (of size nSample) from the chain generated using MCMC
#   nSample         <- 100
#   nStep           <- (nChain-nBurnin) / nSample
#   outputSample    <- array( 0, c(nSample,NDAYS,NOUT) )
#   ii              <- 0   
#   for (j in seq(nBurnin+nStep, nChain, nStep)) {
#     ii <- ii+1
#     params_j           <- pChain[j,] * sc
#     params[ip_BC_s]    <- params_j[icol_pChain_s]
#     outputSample[ii,,] <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
# 	            	                 calendar_prunT, calendar_thinT, NDAYS )
#   } # end of sample loop
# # Analyse the posterior output sample: calculate quantiles 5% and 95%
#   q5  <- sapply( 1:NOUT, function(i) sapply(1:NDAYS,function(j)quantile(outputSample[,j,i],0.05)) )
#   q95 <- sapply( 1:NOUT, function(i) sapply(1:NDAYS,function(j)quantile(outputSample[,j,i],0.95)) )

# Plot
  dev.set()
  if( s < 10 ) {
    png( paste0('BC/results/BC_outputs_data_0',s,'.png'), width=1500, height=1000 )
    } else {
    png( paste0('BC/results/BC_outputs_data_',s,'.png'), width=1500, height=1000 )
  }
    
  # list_runs <- list( outputPriorMode, outputMAP, outputMaxL, q5, q95 )
  # list_runs <- list( outputMAP, outputMaxL, q5, q95 )
  list_runs <- list( outputMAP, outputMaxL )
  plot_outputs_data_s_PNG(
    isite       = s,
    list_runs   = list_runs,
    leg_title   = "BC",
  # leg         = c("Prior","MAP","MaxL"),
  # cols        = c( "red", "black", "green", "black", "black" ),
  # lwds        = c( 3, 3, 2, 2, 2 ),
  # ltys        = c( 1, 1, 1, 3, 3 ) )   
    leg         = c( "MAP","MaxL"),
    cols        = c( "black", "green", "black", "black" ),
    lwds        = c( 3, 2, 2, 2 ),
    ltys        = c( 1, 1, 3, 3 ) )
  
  dev.off()   
}
