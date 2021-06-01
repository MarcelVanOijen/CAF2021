## initialise_CAF2021_general.R ##
## MvO, 2021-05-20

################################################################################
calendar_fert  <- matrix( -1, nrow=100, ncol=3 )
calendar_prunC <- matrix( -1, nrow=100, ncol=3 )
calendar_prunT <- array( -1, c(3,100,3) )
calendar_thinT <- array( -1, c(3,100,3) )

################################################################################
### 1. MODEL LIBRARY FILE & FUNCTION FOR RUNNING THE MODEL
################################################################################
run_model <- function(p     = params,
                      w     = matrix_weather,
					  calf  = calendar_fert,
					  calpC = calendar_prunC,
					  calpT = calendar_prunT,
					  caltT = calendar_thinT,
                      n     = NDAYS) {
  .Fortran('CAF2021', p,w,calf,calpC,calpT,caltT,n,NOUT,
                      matrix(0,n,NOUT))[[9]]
}

################################################################################
### 2. FUNCTION FOR READING WEATHER DATA
################################################################################
read_weather_CAF <- function(y = year_start,
                             d = doy_start,
                             n = NDAYS,
                             f = file_weather) {
  df_weather            <- read.table( f, header=TRUE )
  row_start             <- 1
  while( df_weather[row_start,]$year< y ) { row_start <- row_start+1 }
  while( df_weather[row_start,]$doy < d ) { row_start <- row_start+1 }
  df_weather_sim        <- df_weather[row_start:(row_start+n-1),]
  NMAXDAYS              <- as.integer(10000)
  NWEATHER              <- as.integer(8)
  matrix_weather        <- matrix( 0., nrow=NMAXDAYS, ncol=NWEATHER )
  matrix_weather[1:n,1] <- df_weather_sim$year
  matrix_weather[1:n,2] <- df_weather_sim$doy
  matrix_weather[1:n,3] <- df_weather_sim$GR
  matrix_weather[1:n,4] <- df_weather_sim$TMIN
  matrix_weather[1:n,5] <- df_weather_sim$TMAX
  matrix_weather[1:n,6] <- df_weather_sim$VP
  matrix_weather[1:n,7] <- df_weather_sim$WN
  matrix_weather[1:n,8] <- df_weather_sim$RAIN
  return(matrix_weather)
}

################################################################################
### 3. OUTPUT VARIABLES
################################################################################
nc <- 6 ; nt <- 3

yNames <- yUnits <- list()

yNames[[1]]  <- c( "Time", "year", "doy" )
yUnits[[1]]  <- c( "(y)" , "(y)" , "(d)" )
                                    
yNames[[2]]  <- c( paste0("Ac(",1:nc,")")      , paste0("At(",1:nt,")"),
                   paste0("fNgrowth(",1:nc,")"), paste0("fTran(",1:nc,")") )
yUnits[[2]]  <- c( rep("(m2 c m-2)",nc )       , rep("(m2 t m-2)",nt),
                   rep("(-)",nc)               , rep("(-)",nc) )

yNames[[3]]  <- c( "Cabg_f"         , paste0("harvCP(",1:nc,")"),
                   "harvDM_f_hay"   , paste0("LAI(",1:nc,")") )
yUnits[[3]]  <- c( "(kgC m-2)"      , rep("(kgC m-2 c)",nc),
                   "(kgDM ha-1 y-1)", rep("(m2 m-2 c)",nc) )

yNames[[4]]  <- c( "CabgT_f"              , paste0("CAtree_t(",1:nt,")"),
                   paste0("h_t(",1:nt,")"), paste0("LAIT_c(",1:nc,")"),
                   paste0("treedens_t(",1:nt,")") )
yUnits[[4]]  <- c( "(kgC m-2)"            , rep("(m2 tree-1)",nt),
                   rep("(m)",nt)          , rep("(m2 m-2 c)",nc),
                   rep("(m-2)",nt) )

yNames[[5]]  <- c( "Csoil_f"  , "Nsoil_f"   )
yUnits[[5]]  <- c( "(kgC m-2)", "(kgN m-2)" )

yNames[[6]]  <- c( paste0("CST_t(",1:nt,")"), paste0("SAT_t(",1:nt,")") )
yUnits[[6]]  <- c( rep("(kgC m-2)",nt)      , rep("(m2 m-2)",nt) )

yNames[[7]]  <- c( "Nfert_f"   , "NfixT_f"    , "NsenprunT_f",
                   "Nsenprun_f", "Nleaching_f", "Nemission_f",
                   "Nrunoff_f" , "Nupt_f"     , "NuptT_f" )
yUnits[[7]]  <- rep("(kgN m-2 d-1)",9)
                  
yNames[[8]]  <- c( paste0("CLITT(",1:nc,")")    , paste0("NLITT(",1:nc,")"),
                   paste0("harvCST_t(",1:nt,")"), paste0("harvNST_t(",1:nt,")") )
yUnits[[8]]  <- c( rep("(kgC m-2 c)",nc)        , rep("(kgN m-2 c)",nc),
                   rep("(kgC m-2 d-1)",nt)      , rep("(kgN m-2 d-1)",nt) )
                  
yNames[[9]]  <- c( "CsenprunT_f", "Csenprun_f" , "Rsoil_f", "Crunoff_f" )
yUnits[[9]]  <- rep("(kgC m-2 d-1)",4)

yNames[[10]] <- c( "WA_f"  ,
                   "Rain_f", "Drain_f", "Runoff_f" , "Evap_f"    ,
                   "Tran_f", "TranT_f", "Rainint_f", "RainintT_f" )
yUnits[[10]] <- c( "(mm)"  , rep("(mm d-1)",8) )
                  
yNames[[11]] <- c( "C_f"      ,  "gC_f", "dC_f", "prunC_f", "harvCP_f" ) 
yUnits[[11]] <- c( "(kgC m-2)", rep("(kgC m-2 d-1)",4) )

yNames[[12]] <- c( "CT_f"     , "gCT_f", "harvCBT_f", "harvCPT_f", "harvCST_f" )
yUnits[[12]] <- c( "(kgC m-2)", rep("(kgC m-2 d-1)",4) )

yNames[[13]] <- c( paste0("CPT_t(",1:nt,")"), paste0("harvCPT_t(",1:nt,")"),
                   paste0("harvNPT_t(",1:nt,")") )
yUnits[[13]] <- c( rep("(kgC m-2)",nt)      , rep("(kgC m-2 d-1)",nt)      ,
                   rep("(kgN m-2 d-1)",nt) )

yNames[[14]] <- c( "DVS(1)", "SINKP(1)", "SINKPMAXnew(1)", "DayFl(1)", "PARMA(1)",
                   "DVS(2)", "SINKP(2)", "SINKPMAXnew(2)", "DayFl(2)", "PARMA(2)" )
yUnits[[14]] <- c( rep("(-)",4), "(MJ m-2 d-1)",
                   rep("(-)",4), "(MJ m-2 d-1)" )

yNames[[15]] <- c( "CR_f", "CW_f", "CL_f", "CP_f",
                   paste0("CRT_t(",1:nt,")") , paste0("CBT_t(",1:nt,")"),
                   paste0("CLT_t(",1:nt,")") )
yUnits[[15]] <- c( rep("(kgC m-2)",4),
                   rep("(kgC m-2)",nt)       , rep("(kgC m-2)",nt),
                   rep("(kgC m-2)",nt) )

yNames[[16]] <- c( paste0("LAIT_t(",1:nt,")"), paste0("fTranT_t(",1:nt,")") )
yUnits[[16]] <- c( rep("(m2 t m-2)",nt)      , rep("(-)",nt) )

yNames[[17]] <- c( "D_Csoil_f_hay", "D_Csys_f_hay", "D_Nsoil_f_hay" )
yUnits[[17]] <- c( rep("(kgC ha-1 y-1)",2)        , "(kgN ha-1 y-1)" )

yNames[[18]] <- c( "NfixT_f_hay", "Nleaching_f_hay" )
yUnits[[18]] <- rep( "(kgN ha-1 y-1)", 2 )

yNames[[19]] <- "Shade_f"
yUnits[[19]] <- "(-)"

outputNames  <- unlist(yNames) ; outputUnits <- unlist(yUnits)
NOUT         <- as.integer( length(outputNames) )

################################################################################
### 4. FUNCTION FOR CHANGING PARAMETER VALUES
################################################################################
set_par <- function( names=names_params[1], vals=params[1] ) {
  ipar <- match(names,names_params) ; npar <- length(ipar)
  for (i in 1:npar) { params[ipar[i]] <- vals[i] }
  return(params)
}

################################################################################
### 5. FUNCTIONS FOR EXPORTING THE RESULTS TO FILE (pdf with plots, txt with table)
################################################################################
plot_output <- function(
  list_output = list(output),
  vars        = outputNames[-(1:3)],
  leg         = paste( "Run", 1:length(list_output) ),
  leg_title   = "LEGEND",
  nrow_plot   = ceiling( sqrt((length(vars)+1) * 8/11) ),
  ncol_plot   = ceiling( (length(vars)+1)/nrow_plot ),
  lty         = rep(1,length(list_output)),
  lwd         = rep(3,length(list_output))
) {
  par( mfrow=c(nrow_plot,ncol_plot), mar=c(2, 2, 2, 1) )
  if (!is.list(list_output)) list_output <- list(list_output) ; nlist <- length(list_output)
  col_vars <- match(vars,outputNames)                         ; nvars <- length(vars)
  for (iv in 1:nvars) {
    c       <- col_vars[iv]
    g_range <- range( sapply( 1:nlist, function(il){range(list_output[[il]][,c])} ) )
    plot( list_output[[1]][,1], list_output[[1]][,c],
          xlab="", ylab="", cex.main=1,
          main=paste(outputNames[c]," ",outputUnits[c],sep=""),
          type='l', col=1, lty=lty[1], lwd=lwd[1], ylim=g_range )
    if (nlist >= 2) {
      for (il in 2:nlist) {
        points( list_output[[il]][,1], list_output[[il]][,c],
                col=il, type='l', lty=lty[il], lwd=lwd[il] )
      }
    }
    if ( (iv%%(nrow_plot*ncol_plot-1)==0) || (iv==nvars) ) {
      plot(1,type='n', axes=FALSE, xlab="", ylab="")
      legend("bottomright", leg, lty=lty, lwd=lwd, col=1:nlist, title = leg_title)
    }
  }
}

table_output <- function(
  list_output = list(output),
  vars        = outputNames[-(1:1)],
  file_table  = paste( "output_", format(Sys.time(),"%H_%M.txt"), sep="" ),
  leg         = paste( "Run", 1:length(list_output) )
) {
  if (!is.list(list_output)) list_output <- list(list_output) ; nlist <- length(list_output)
  col_vars <- match(vars,outputNames)                         ; nvars <- length(vars)
  table_output         <- c("day", list_output[[1]][,1:1] )
  for (il in 1:nlist) {
    table_il     <- if (nvars==1) c    (vars, list_output[[il]][,col_vars]) else
                                  rbind(vars, list_output[[il]][,col_vars])
    table_output <- cbind( table_output, table_il )
  }
  colnames(table_output) <- c( "",rep(leg,each=nvars) )
  write.table( table_output, file_table, sep="\t", row.names=F )
}

################################################################################
### 6. FUNCTIONS FOR ANALYSIS

#######################
### 6.1 Function 'SA()'
#######################
SA <- function( parname_SA = "KEXT",
                pmult      = 2^(-1:1),
                vars       = outputNames[-(1:1)],
                leg_title  = parname_SA,
                nrow_plot  = ceiling( sqrt((length(vars)+1) * 8/11) ),
                ncol_plot  = ceiling( (length(vars)+1)/nrow_plot ),
                lty        = rep(1,length(pmult)),
                lwd        = rep(3,length(pmult)),
                file_init  = "initialisation/initialise_CAF2021_Turrialba_01_E_AC.R",
                file_plot  = paste("SA_",parname_SA,format(Sys.time(),"_%H_%M.pdf"),sep="")
) {
  source(file_init)
  cat( "SA initialised for:", substr(basename(file_init),1,nchar(basename(file_init))-2), "\n")
  ip_SA          <- match( parname_SA, row.names(df_params) )
  par_SA_default <- params[ip_SA]
  nmult          <- length(pmult)
  list_output    <- vector( "list", nmult )
  for (im in 1:nmult) {
    params[ip_SA]     <- par_SA_default * pmult[im]
    list_output[[im]] <- run_model(params)
  }
  pdf( file_plot, paper="a4r", width=11, height=8 )
  plot_output( list_output, vars=vars,
               leg=as.character(pmult*par_SA_default), leg_title=parname_SA,
               nrow_plot=nrow_plot, ncol_plot=ncol_plot, lty=lty, lwd=lwd )
  dev.off()
  table_output(list_output, vars=vars,
               file_table=paste("SA_",parname_SA,format(Sys.time(),"_%H_%M.txt"),sep=""),
               leg=paste(parname_SA,"=",pmult*par_SA_default,sep=""))
}

##############################
### 6.2 Function 'SA_BC()'
##############################
SA_BC <- function(
  parname_SA    = "KEXT",
  pmult         = 2^(-1:1),
  vars          = outputNames[-(1:3)],
  leg_title     = parname_SA,
  nrow_plot     = ceiling( sqrt((length(vars)+1) * 8/11) ),
  ncol_plot     = ceiling( (length(vars)+1)/nrow_plot ),
  lty           = rep(1,length(pmult)),
  lwd           = rep(3,length(pmult)),
  file_init_BC  = "BC/BC_CAF2021_MCMC_init_Turrialba_5&6.R",
  file_par      = "CAF2021_parModes.txt",
  partype       = "MAP",
  file_plot_outputs      = paste("SA_BC_outputs"     ,format(Sys.time(),"_%H_%M.pdf"),sep=""),
  file_plot_outputs_data = paste("SA_BC_outputs_data",format(Sys.time(),"_%H_%M.pdf"),sep=""),
  file_plot_likelihoods  = paste("SA_BC_likelihoods" ,format(Sys.time(),"_%H_%M.pdf"),sep="") )
{ source( file_init_BC )
  nmult <- length(pmult)
  cat( "SA initialised for:", substr(basename(file_init_BC),1,nchar(basename(file_init_BC))-2), "\n")
  parheaders  <- paste( partype, "_", as.character(1:nSites), sep="" )
  df_parModes <- read.table( file_par, header=TRUE, sep="\t" )
  # SITE CONDITIONS
  source('BC/BC_CAF2021_MCMC_init_general.R')
  for (s in 1:nSites) {
    params           <- as.matrix( df_parModes[ parheaders[s] ] )
    list_params[[s]] <- params
  }
  ip_SA <- match( parname_SA, row.names(df_params) )
  ivar             <- unique( unlist(data_index) )
  names_var        <- outputNames[ivar]
  nvar             <- length(ivar)
  df_logL          <- data.frame( matrix(nrow=nSites,ncol=nvar) )
  names(df_logL)   <- names_var
  list_df_logL     <- vector("list",nmult)
  for(i in 1:nmult) list_df_logL[[i]] <- df_logL

  pdf( file_plot_outputs_data, paper="a4r", width=11, height=8 )
  dev_outputs_data <- dev.cur()
  pdf( file_plot_outputs     , paper="a4r", width=11, height=8 )
  dev_outputs      <- dev.cur()
  pdf( file_plot_likelihoods , paper="a4r", width=11, height=8 )
  dev_likelihoods  <- dev.cur()

  for (s in 1:nSites) {
    params         <- list_params        [[s]] ; matrix_weather <- list_matrix_weather[[s]]
    calendar_fert  <- list_calendar_fert [[s]] ; calendar_prunC <- list_calendar_prunC[[s]]
    calendar_prunT <- list_calendar_prunT[[s]] ; calendar_thinT <- list_calendar_thinT[[s]]
    NDAYS          <- list_NDAYS         [[s]]
    par_SA_default <- params[ip_SA]
    list_output    <- vector( "list", nmult )
    for (g in 1:nmult) {
      params[ip_SA]    <- par_SA_default * pmult[g]
      output           <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                                     calendar_prunT, calendar_thinT, NDAYS )
      list_output[[g]] <- output
      for(v in 1:nvar) {
        logLi_s                   <- calc_logLi_s( s, output=output )
        rows_var_s                <- which( logLi_s[,1]==ivar[v] )
        list_df_logL[[g]][[v]][s] <- sum( logLi_s[rows_var_s,2] )
      }
    }
    dev.set(dev_outputs_data)
    leg_title_outputs_data <- parname_SA
    leg                    <- as.character(pmult*par_SA_default)
    plot_outputs_data_s( isite        = s,
                         list_runs    = list_output,
                         leg_title    = leg_title_outputs_data,
                         leg          = leg,
                         cols         = 1:nmult,
                         lwds         = lwd,
                         ltys         = lty )
    dev.set(dev_outputs)
    leg_title_outputs <- paste( "Site: ", s, ", par: ", parname_SA, sep="")
    par( omi=c(0,0,0.3,0), mar=c(2,2,2,1) )
    plot_output( list_output = list_output,
                 vars        = vars,
                 leg_title   = leg_title_outputs,
                 leg         = leg,
                 nrow_plot   = nrow_plot, ncol_plot = ncol_plot,
                 lty         = lty      , lwd       = lwd )
    sitenames <- gsub( ".R", "", sub(".*CAF2021_","",sitesettings_filenames) )
    mtext( paste("SITE ",s," (",sitenames[s],"), ","par: ",parname_SA, sep=""),
           side=3, line=1, outer=TRUE, cex=1, font=2)
  }
  dev.off(dev_outputs_data)
  dev.off(dev_outputs     )

  dev.set(dev_likelihoods )
  nrow_plot_L <- ceiling( sqrt((nSites+3) * 8/11) )
  ncol_plot_L <- ceiling( (nSites+3)/nrow_plot_L )
  par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )

  vector_logLtot <- rep( NA, nmult )
  for(g in 1:nmult) vector_logLtot[g] <- sum( list_df_logL[[g]], na.rm=TRUE )
  barplot( vector_logLtot-min(vector_logLtot),
           main=paste("SUM ALL SITES"), col=1:nmult )
  for(s in 1:nSites) {
    vector_logL <- rep(NA,nmult)
    for(g in 1:nmult) {
      vector_logL[g] <- rowSums(list_df_logL[[g]]) [s]
    }
    barplot( vector_logL-min(vector_logL), main=paste("site",s),
             ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
             col=1:nmult )
  }
  plot(1,type='n', axes=FALSE, xlab="", ylab="")
  legend( "bottomright", title=parname_SA, leg=leg, fill=1:nmult )

  for(v in 1:nvar) {
    par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )
    vector_logLtot <- rep(NA,nmult)
    for(g in 1:nmult) {
      vector_logLtot[g] <- sum( list_df_logL[[g]][[v]], na.rm=TRUE )
    }
    barplot( vector_logLtot-min(vector_logLtot),
             main=paste(names_var[v],":","SUM ALL SITES"), col=1:nmult )
    vector_logL    <- rep(NA,nmult)
    for (s in 1:nSites) {
      for(g in 1:nmult) {
        vector_logL[g] <- list_df_logL[[g]][[v]] [s]
      }
      barplot( vector_logL-min(vector_logL),
               main=paste(names_var[v],":","site",s),
               ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
               col=1:nmult )
    }
    plot(1,type='n', axes=FALSE, xlab="", ylab="")
    legend( "bottomright", title=parname_SA, leg=leg, fill=1:nmult )
  }
  dev.off(dev_likelihoods )
} # End of function 'SA_BC()'
