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
### 2. FUNCTIONS FOR READING WEATHER DATA
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

read_listweather_CAF <- function( list_w=weather_C.f, f=1,
                                  y1=year_start, d1=doy_start, nd=NDAYS) {
  file_weather <- "tmp_weather.txt"
  write.table( list_w[[f]], file=file_weather,
               col.names=colnames( list_w[[f]] ) )
  matrix_weather <- read_weather_CAF( y1, d1, nd, file_weather )
  return( matrix_weather ) 
}

################################################################################
### 3. OUTPUT VARIABLES
################################################################################
nc <- 6 ; nt <- 3 ; nz <- nt*2

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

yNames[[14]] <- c( "DayFl",
                   "DVS(1)", "SINKP(1)", "SINKPMAXnew(1)", "PARMA(1)",
                   "DVS(2)", "SINKP(2)", "SINKPMAXnew(2)", "PARMA(2)" )
yUnits[[14]] <- c( "(-)",
                   rep("(-)",3), "(MJ m-2 d-1)",
                   rep("(-)",3), "(MJ m-2 d-1)" )

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

yNames[[20]] <- paste0("z(",1:nz,")")
yUnits[[20]] <- rep("(-)",nz)

yNames[[21]] <- "f3up"
yUnits[[21]] <- "(-)"

yNames[[22]] <- paste0("DayHarv(",1:nc,")")
yUnits[[22]] <- rep("(-)",nc)

yNames[[23]] <- paste0("fNgrowth_t(",1:nt,")")
yUnits[[23]] <- rep("(-)",nt)

outputNames  <- unlist(yNames) ; outputUnits <- unlist(yUnits)
NOUT         <- as.integer( length(outputNames) )

# cbind( outputNames, outputUnits )

################################################################################
### 4. FUNCTIONS FOR SETTING PARAMETER VALUES
################################################################################
set_par <- function( names=names_params[1], vals=params[1], p.old=params ) {
  ipar  <- match(names,names_params) ; npar <- length(ipar)
  p.new <- p.old
  for (i in 1:npar) { p.new[ipar[i]] <- vals[i] }
  return( p.new )
}

set_par_speciesT <- function( it, species="E. poeppigiana", p.old=params ) {
  names <- vals <- NULL
  if (species %in% c("Erythrina","Erythrina poeppigiana","E. poeppigiana",
                     "Poro","poro")) {
    names[ 1] <- paste0("CBtree0("  ,it,")") ; vals[ 1] <-     0.1
    names[ 2] <- paste0("CLtree0("  ,it,")") ; vals[ 2] <-     0.1
    names[ 3] <- paste0("CRtree0("  ,it,")") ; vals[ 3] <-     0.1
    names[ 4] <- paste0("CStree0("  ,it,")") ; vals[ 4] <-     0.1
    names[ 5] <- paste0("FPT("      ,it,")") ; vals[ 5] <-     0
    names[ 6] <- paste0("FST("      ,it,")") ; vals[ 6] <-     0.55
    names[ 7] <- paste0("FWT("      ,it,")") ; vals[ 7] <-     0.83
    names[ 8] <- paste0("HMAX("     ,it,")") ; vals[ 8] <-     6
    names[ 9] <- paste0("KAC("      ,it,")") ; vals[ 9] <-     9.1
    names[10] <- paste0("KACEXP("   ,it,")") ; vals[10] <-     0.6
    names[11] <- paste0("KH("       ,it,")") ; vals[11] <-     3.7
    names[12] <- paste0("KHEXP("    ,it,")") ; vals[12] <-     0.25
    names[13] <- paste0("KNFIX("    ,it,")") ; vals[13] <-     0.05
    names[14] <- paste0("TBEFOREPT(",it,")") ; vals[14] <-  1825
    names[15] <- paste0("TCBT("     ,it,")") ; vals[15] <-  2100
    names[16] <- paste0("TCRT("     ,it,")") ; vals[16] <-  4000
    names[17] <- paste0("TCST("     ,it,")") ; vals[17] <- 99999
    names[18] <- paste0("TOPTT("    ,it,")") ; vals[18] <-    24.4
    names[19] <- paste0("TTOLT("    ,it,")") ; vals[19] <-     8
    p.new <- set_par( names, vals, p.old=p.old )    
  } else if (species %in% c("Inga","Inga sp.")) {
    names[ 1] <- paste0("CBtree0("  ,it,")") ; vals[ 1] <-     0.1
    names[ 2] <- paste0("CLtree0("  ,it,")") ; vals[ 2] <-     0.1
    names[ 3] <- paste0("CRtree0("  ,it,")") ; vals[ 3] <-     0.1
    names[ 4] <- paste0("CStree0("  ,it,")") ; vals[ 4] <-     0.1
    names[ 5] <- paste0("FPT("      ,it,")") ; vals[ 5] <-     0
    names[ 6] <- paste0("FST("      ,it,")") ; vals[ 6] <-     0.42
    names[ 7] <- paste0("FWT("      ,it,")") ; vals[ 7] <-     0.65
    names[ 8] <- paste0("HMAX("     ,it,")") ; vals[ 8] <-    10
    names[ 9] <- paste0("KAC("      ,it,")") ; vals[ 9] <-     6.0
    names[10] <- paste0("KACEXP("   ,it,")") ; vals[10] <-     0.5
    names[11] <- paste0("KH("       ,it,")") ; vals[11] <-     5
    names[12] <- paste0("KHEXP("    ,it,")") ; vals[12] <-     0.25
    names[13] <- paste0("KNFIX("    ,it,")") ; vals[13] <-     0.05
    names[14] <- paste0("TBEFOREPT(",it,")") ; vals[14] <-  1825
    names[15] <- paste0("TCBT("     ,it,")") ; vals[15] <-  1100
    names[16] <- paste0("TCRT("     ,it,")") ; vals[16] <-  3400
    names[17] <- paste0("TCST("     ,it,")") ; vals[17] <- 99999
    names[18] <- paste0("TOPTT("    ,it,")") ; vals[18] <-    25.9
    names[19] <- paste0("TTOLT("    ,it,")") ; vals[19] <-     8
    p.new <- set_par( names, vals, p.old=p.old )    
  } else if (species %in% c("Banana", "banana",
                            "Musa sp.","Musa")) {
    names[ 1] <- paste0("CBtree0("  ,it,")") ; vals[ 1] <-    1
    names[ 2] <- paste0("CLtree0("  ,it,")") ; vals[ 2] <-    1
    names[ 3] <- paste0("CRtree0("  ,it,")") ; vals[ 3] <-    1
    names[ 4] <- paste0("CStree0("  ,it,")") ; vals[ 4] <-    1
    names[ 5] <- paste0("FPT("      ,it,")") ; vals[ 5] <-    0.5
    names[ 6] <- paste0("FST("      ,it,")") ; vals[ 6] <-    0.5
    names[ 7] <- paste0("FWT("      ,it,")") ; vals[ 7] <-    0.5
    names[ 8] <- paste0("HMAX("     ,it,")") ; vals[ 8] <-    6
    names[ 9] <- paste0("KAC("      ,it,")") ; vals[ 9] <-    7
    names[10] <- paste0("KACEXP("   ,it,")") ; vals[10] <-    0.6
    names[11] <- paste0("KH("       ,it,")") ; vals[11] <-    4.5
    names[12] <- paste0("KHEXP("    ,it,")") ; vals[12] <-    0.42
    names[13] <- paste0("KNFIX("    ,it,")") ; vals[13] <-    0
    names[14] <- paste0("TBEFOREPT(",it,")") ; vals[14] <-  100
    names[15] <- paste0("TCBT("     ,it,")") ; vals[15] <-  365
    names[16] <- paste0("TCRT("     ,it,")") ; vals[16] <-  365
    names[17] <- paste0("TCST("     ,it,")") ; vals[17] <-  365
    names[18] <- paste0("TOPTT("    ,it,")") ; vals[18] <-   22
    names[19] <- paste0("TTOLT("    ,it,")") ; vals[19] <-    8
    p.new <- set_par( names, vals, p.old=p.old )    
  } else if (species %in% c("Avocado", "avocado",
                            "Persea americana","P. americana")) {
    names[ 1] <- paste0("CBtree0("  ,it,")") ; vals[ 1] <-    0.1
    names[ 2] <- paste0("CLtree0("  ,it,")") ; vals[ 2] <-    0.1
    names[ 3] <- paste0("CRtree0("  ,it,")") ; vals[ 3] <-    0.1
    names[ 4] <- paste0("CStree0("  ,it,")") ; vals[ 4] <-    0.1
    names[ 5] <- paste0("FPT("      ,it,")") ; vals[ 5] <-    0.5
    names[ 6] <- paste0("FST("      ,it,")") ; vals[ 6] <-    0.5
    names[ 7] <- paste0("FWT("      ,it,")") ; vals[ 7] <-    0.5
    names[ 8] <- paste0("HMAX("     ,it,")") ; vals[ 8] <-    6
    names[ 9] <- paste0("KAC("      ,it,")") ; vals[ 9] <-    4
    names[10] <- paste0("KACEXP("   ,it,")") ; vals[10] <-    0.6
    names[11] <- paste0("KH("       ,it,")") ; vals[11] <-    3
    names[12] <- paste0("KHEXP("    ,it,")") ; vals[12] <-    0.4
    names[13] <- paste0("KNFIX("    ,it,")") ; vals[13] <-    0
    names[14] <- paste0("TBEFOREPT(",it,")") ; vals[14] <- 1825
    names[15] <- paste0("TCBT("     ,it,")") ; vals[15] <- 1000
    names[16] <- paste0("TCRT("     ,it,")") ; vals[16] <- 1000
    names[17] <- paste0("TCST("     ,it,")") ; vals[17] <- 1000
    names[18] <- paste0("TOPTT("    ,it,")") ; vals[18] <-   20
    names[19] <- paste0("TTOLT("    ,it,")") ; vals[19] <-    8
    p.new <- set_par( names, vals, p.old=p.old )    
  } else if (species %in% c("Grevillea", "G. robusta")) {
    names[ 1] <- paste0("CBtree0("  ,it,")") ; vals[ 1] <-     0.1
    names[ 2] <- paste0("CLtree0("  ,it,")") ; vals[ 2] <-     0.1
    names[ 3] <- paste0("CRtree0("  ,it,")") ; vals[ 3] <-     0.1
    names[ 4] <- paste0("CStree0("  ,it,")") ; vals[ 4] <-     0.1
    names[ 5] <- paste0("FLTMAX("   ,it,")") ; vals[ 5] <-     0.27
    names[ 6] <- paste0("FPT("      ,it,")") ; vals[ 6] <-     0
    names[ 7] <- paste0("FST("      ,it,")") ; vals[ 7] <-     0.25
    names[ 8] <- paste0("FWT("      ,it,")") ; vals[ 8] <-     0.43
    names[ 9] <- paste0("HMAX("     ,it,")") ; vals[ 9] <-    50
    names[10] <- paste0("KAC("      ,it,")") ; vals[10] <-     6.7
    names[11] <- paste0("KACEXP("   ,it,")") ; vals[11] <-     0.76
    names[12] <- paste0("KH("       ,it,")") ; vals[12] <-     5
    names[13] <- paste0("KHEXP("    ,it,")") ; vals[13] <-     0.25
    names[14] <- paste0("KNFIX("    ,it,")") ; vals[14] <-     0
    names[15] <- paste0("LAIMAXT("  ,it,")") ; vals[15] <-     5
    names[16] <- paste0("SLAT("     ,it,")") ; vals[16] <-    25
    names[17] <- paste0("TBEFOREPT(",it,")") ; vals[17] <-  1825
    names[18] <- paste0("TCBT("     ,it,")") ; vals[18] <-  2600
    names[19] <- paste0("TCRT("     ,it,")") ; vals[19] <-  5200
    names[20] <- paste0("TCST("     ,it,")") ; vals[20] <- 99999
    names[21] <- paste0("TOPTT("    ,it,")") ; vals[21] <-    25.0
    names[22] <- paste0("TTOLT("    ,it,")") ; vals[22] <-     8
    p.new <- set_par( names, vals, p.old=p.old )    
  } else if (species %in% c("Cordia", "C. alliodora")) {
    names[ 1] <- paste0("CBtree0("  ,it,")") ; vals[ 1] <-     0.1
    names[ 2] <- paste0("CLtree0("  ,it,")") ; vals[ 2] <-     0.1
    names[ 3] <- paste0("CRtree0("  ,it,")") ; vals[ 3] <-     0.1
    names[ 4] <- paste0("CStree0("  ,it,")") ; vals[ 4] <-     0.1
    names[ 5] <- paste0("FLTMAX("   ,it,")") ; vals[ 5] <-     0.24
    names[ 6] <- paste0("FPT("      ,it,")") ; vals[ 6] <-     0
    names[ 7] <- paste0("FST("      ,it,")") ; vals[ 7] <-     0.3
    names[ 8] <- paste0("FWT("      ,it,")") ; vals[ 8] <-     0.55
    names[ 9] <- paste0("HMAX("     ,it,")") ; vals[ 9] <-    50
    names[10] <- paste0("KAC("      ,it,")") ; vals[10] <-     6.7
    names[11] <- paste0("KACEXP("   ,it,")") ; vals[11] <-     0.84
    names[12] <- paste0("KH("       ,it,")") ; vals[12] <-     5.6
    names[13] <- paste0("KHEXP("    ,it,")") ; vals[13] <-     0.33
    names[14] <- paste0("KNFIX("    ,it,")") ; vals[14] <-     0
    names[15] <- paste0("LAIMAXT("  ,it,")") ; vals[15] <-     5
    names[16] <- paste0("SLAT("     ,it,")") ; vals[16] <-    25
    names[17] <- paste0("TBEFOREPT(",it,")") ; vals[17] <-  1825
    names[18] <- paste0("TCBT("     ,it,")") ; vals[18] <-  2600
    names[19] <- paste0("TCRT("     ,it,")") ; vals[19] <-  5200
    names[20] <- paste0("TCST("     ,it,")") ; vals[20] <- 99999
    names[21] <- paste0("TOPTT("    ,it,")") ; vals[21] <-    25.0
    names[22] <- paste0("TTOLT("    ,it,")") ; vals[22] <-     8
    p.new <- set_par( names, vals, p.old=p.old )    
  } else { stop( paste("Parameterisation for", species, "is not provided.") ) }
  return( p.new )
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
### 6. FUNCTIONS FOR ANALYSIS OF MODEL OUTPUT
################################################################################

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

##############################################
### 6.3 Functions for bioegeochemical balances
##############################################
plot_NbalanceSoil <- function( sim=output, title1="", title2=title1,
                               ymax=NULL, verify=F ) {
  NinNames  <- c("Nfert_f", "NfixT_f", "NsenprunT_f", "Nsenprun_f")
  NoutNames <- c("Nleaching_f", "Nemission_f", "Nrunoff_f", "Nupt_f", "NuptT_f")
  nNin      <- length(NinNames) ; nNout  <- length(NoutNames)
  i.Nin     <- rep(NA,nNin)     ; i.Nout <- rep(NA,nNout)
  for(i in 1:nNin ) {i.Nin [i] <- which( outputNames==NinNames [i] ) }
  for(i in 1:nNout) {i.Nout[i] <- which( outputNames==NoutNames[i] ) }
  NinMean   <- colMeans(sim[,i.Nin])  * 1.e4 * 365 # kgN ha-1 y-1
  NoutMean  <- colMeans(sim[,i.Nout]) * 1.e4 * 365 # kgN ha-1 y-1
  if(is.null(ymax)) {
    y.max <- max( sum(NinMean), sum(NoutMean) )
  } else {
    y.max = ymax
  }
  leg <- NinNames
  barplot( as.matrix(NinMean),
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Inputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title1,adj=1)
  par(mar=c(3,0,3,3))
  leg <- NoutNames
  barplot( as.matrix(NoutMean),
           yaxt="n",
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Outputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title2,adj=0)
  if(verify) {
    Nsoil_f      <- sim[ , which( outputNames=="Nsoil_f") ] # kgN m-2
    D_Nsoil_f    <- Nsoil_f[NDAYS] - Nsoil_f[1]                # kgN m-2
    NinSum       <- colSums(sim[2:NDAYS,i.Nin ])            # kgN m-2
    NoutSum      <- colSums(sim[2:NDAYS,i.Nout])            # kgN m-2
    NinMinusNout <- sum(NinSum) - sum(NoutSum)
    cat( "VERIFYING THE N-BALANCE FOR THE SOIL (kgN m-2):",
         "\nState variable change: ", D_Nsoil_f,
         "\nInputs minus outputs : ", NinMinusNout )
  }
}
##############################################
plot_CbalanceSoil <- function( sim=output, title1="", title2=title1,
                               ymax=NULL, verify=F ) {
  CinNames  <- c("CsenprunT_f", "Csenprun_f")
  CoutNames <- c("Rsoil_f", "Crunoff_f")
  nCin      <- length(CinNames) ; nCout  <- length(CoutNames)
  i.Cin     <- rep(NA,nCin)     ; i.Cout <- rep(NA,nCout)
  for(i in 1:nCin ) {i.Cin [i] <- which( outputNames==CinNames [i] ) }
  for(i in 1:nCout) {i.Cout[i] <- which( outputNames==CoutNames[i] ) }
  CinMean   <- colMeans(sim[,i.Cin])  * 1.e4 * 365 # kgC ha-1 y-1
  CoutMean  <- colMeans(sim[,i.Cout]) * 1.e4 * 365 # kgC ha-1 y-1
  if(is.null(ymax)) {
    y.max <- max( sum(CinMean), sum(CoutMean) )
  } else {
    y.max = ymax
  }
  leg <- CinNames
  barplot( as.matrix(CinMean),
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Inputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title1,adj=1)
  par(mar=c(3,0,3,3))
  leg <- CoutNames
  barplot( as.matrix(CoutMean),
           yaxt="n",
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Outputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title2,adj=0)
  if(verify) {
    Csoil_f   <- sim[ , which( outputNames=="Csoil_f") ] # kgC m-2 
    D_Csoil_f <- Csoil_f[NDAYS] - Csoil_f[1]             # kgC m-2
    CinSum        <- colSums(sim[2:NDAYS,i.Cin ])        # kgC m-2
    CoutSum       <- colSums(sim[2:NDAYS,i.Cout])        # kgC m-2
    CinMinusCout  <- sum(CinSum) - sum(CoutSum)
    cat( "VERIFYING THE C-BALANCE FOR THE SOIL (kgC m-2):",
         "\nState variable change: ", D_Csoil_f,
         "\nInputs minus outputs : ", CinMinusCout )
  }
}
##############################################
plot_H2ObalanceSoil <- function( sim=output, title1="", title2=title1,
                                 ymax=NULL, verify=F ) {
  WinNames  <- c("Rain_f")
  WoutNames <- c("Drain_f", "Runoff_f" , "Evap_f"    ,"Tran_f" , 
                 "TranT_f", "Rainint_f", "RainintT_f")
  nWin      <- length(WinNames) ; nWout  <- length(WoutNames)
  i.Win     <- rep(NA,nWin)     ; i.Wout <- rep(NA,nWout)
  for(i in 1:nWin ) {i.Win [i] <- which( outputNames==WinNames [i] ) }
  for(i in 1:nWout) {i.Wout[i] <- which( outputNames==WoutNames[i] ) }
  WinMean   <-    mean (sim[,i.Win])  # mm d-1
  WoutMean  <- colMeans(sim[,i.Wout]) # mm d-1
  if(is.null(ymax)) {
    y.max <- max( sum(WinMean), sum(WoutMean) )
  } else {
    y.max = ymax
  }
  leg <- WinNames
  barplot( as.matrix(WinMean),
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Inputs (mm d-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title1,adj=1)
  par(mar=c(3,0,3,3))
  leg <- WoutNames
  barplot( as.matrix(WoutMean),
           yaxt="n",
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Outputs (mm d-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title2,adj=0)
  if(verify) {
    WA_f   <- sim[ , which( outputNames=="WA_f") ] # mm
    D_WA_f <- WA_f[NDAYS] - WA_f[1]                   # mm
    WinSum        <-    sum (sim[2:NDAYS,i.Win ])  # mm
    WoutSum       <- colSums(sim[2:NDAYS,i.Wout])  # mm
    WinMinusWout  <- sum(WinSum) - sum(WoutSum)
    cat( "VERIFYING THE H2O-BALANCE FOR THE SOIL (mm):",
         "\nState variable change: ", D_WA_f,
         "\nInputs minus outputs : ", WinMinusWout )
  }
}
##############################################
plot_CbalanceCoffee <- function( sim=output, title1="", title2=title1,
                                 ymax=NULL, verify=F ) {
  CinNames  <- "gC_f"
  CoutNames <- c("dC_f", "prunC_f", "harvCP_f")
  nCin      <- length(CinNames) ; nCout  <- length(CoutNames)
  i.Cin     <- rep(NA,nCin)     ; i.Cout <- rep(NA,nCout)
  for(i in 1:nCin ) {i.Cin [i] <- which( outputNames==CinNames [i] ) }
  for(i in 1:nCout) {i.Cout[i] <- which( outputNames==CoutNames[i] ) }
  CinMean   <-    mean (sim[,i.Cin])  * 1.e4 * 365 # kgC ha-1 y-1
  CoutMean  <- colMeans(sim[,i.Cout]) * 1.e4 * 365 # kgC ha-1 y-1
  if(is.null(ymax)) {
    y.max <- max( sum(CinMean), sum(CoutMean) )
  } else {
    y.max = ymax
  }
  leg <- CinNames
  barplot( CinMean,
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Inputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title1,adj=1)
  par(mar=c(3,0,3,3))
  leg <- CoutNames
  barplot( as.matrix(CoutMean),
           yaxt="n",
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Outputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title2,adj=0)
  if(verify) {
    C_f          <- sim[ , which( outputNames=="C_f" ) ] # kgC m-2
    D_C_f        <- C_f[NDAYS] - C_f[1]                     # kgC m-2
    CinSum       <-    sum (sim[2:NDAYS,i.Cin ])         # kgC m-2
    CoutSum      <- colSums(sim[2:NDAYS,i.Cout])         # kgC m-2
    CinMinusCout <- sum(CinSum) - sum(CoutSum)
    cat( "VERIFYING THE C-BALANCE OF COFFEE (kgC m-2):",
         "\nState variable change: ", D_C_f,
         "\nInputs minus outputs : ", CinMinusCout )
  }
}
##############################################
plot_CbalanceSystem <- function( sim=output, title1="", title2=title1,
                                 ymax=NULL, verify=F ) {
  CinNames  <- c("gC_f"   , "gCT_f")
  CoutNames <- c("Rsoil_f", "Crunoff_f",
                 "harvCP_f", "harvCPT_f", "harvCBT_f", "harvCST_f")
  nCin      <- length(CinNames) ; nCout  <- length(CoutNames)
  i.Cin     <- rep(NA,nCin)     ; i.Cout <- rep(NA,nCout)
  for(i in 1:nCin ) {i.Cin [i] <- which( outputNames==CinNames [i] ) }
  for(i in 1:nCout) {i.Cout[i] <- which( outputNames==CoutNames[i] ) }
  CinMean   <- colMeans(sim[,i.Cin])  * 1.e4 * 365 # kgC ha-1 y-1
  CoutMean  <- colMeans(sim[,i.Cout]) * 1.e4 * 365 # kgC ha-1 y-1
  if(is.null(ymax)) {
    y.max <- max( sum(CinMean), sum(CoutMean) )
  } else {
    y.max = ymax
  }
  leg <- CinNames
  barplot( as.matrix(CinMean),
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Inputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title1,adj=1)
  par(mar=c(3,0,3,3))
  leg <- CoutNames
  barplot( as.matrix(CoutMean),
           yaxt="n",
           beside=F, ylim=c(0,y.max), col=1:length(leg),
           legend.text=leg,
           args.legend=list( title="Outputs (kg ha-1 y-1)", cex=0.8,
                             x="bottom", bg="white", inset=c(-0.3,0) ) )
  title(title2,adj=0)
  if(verify) {
    C_f      <- sim[ , which( outputNames=="C_f" )    ] # kgC m-2
    CT_f     <- sim[ , which( outputNames=="CT_f")    ] # kgC m-2
    Csoil_f  <- sim[ , which( outputNames=="Csoil_f") ] # kgC m-2 
    Csys_f   <- C_f + CT_f + Csoil_f                    # kgC m-2
    D_Csys_f <- Csys_f[NDAYS] - Csys_f[1]               # kgC m-2
    CinSum       <- colSums(sim[2:NDAYS,i.Cin ])        # kgC m-2
    CoutSum      <- colSums(sim[2:NDAYS,i.Cout])        # kgC m-2
    CinMinusCout <- sum(CinSum) - sum(CoutSum)
    cat( "VERIFYING THE C-BALANCE FOR THE SYSTEM (kgC m-2):",
         "\nState variable change: ", D_Csys_f,
         "\nInputs minus outputs : ", CinMinusCout )
  }
}

#########################################
### 6.4 Functions for carbon distribution
#########################################
plot_CdistributionCoffee <- function( sim=output ) {
  Time <- sim[,1]
  i.CP <- which( outputNames=="CP_f" ) ; CP <- sim[,i.CP]
  i.CL <- which( outputNames=="CL_f" ) ; CL <- sim[,i.CL]
  i.CW <- which( outputNames=="CW_f" ) ; CW <- sim[,i.CW]
  i.CR <- which( outputNames=="CR_f" ) ; CR <- sim[,i.CR]
  plot( Time, CR+CW+CL+CP,
        main="Carbon in coffee (kg C m-2)",
        xlab="", ylab="", type="l", ylim=c(0,max(CR+CW+CL+CP)) )
  points( Time, CR+CW+CL, type="l", col="yellow" )
  points( Time, CR+CW   , type="l", col="green" )
  points( Time, CR      , type="l", col="red" )
  legend( "bottom", cex=0.7, lty=1, bg="white",
          col=c("black","yellow","green","red"),
          legend=c("+ CP: Beans",
                   "+ CL: Leaves",
                   "+ CW: Wood",
                   "   CR: Roots" ) )
}
##############################################
plot_CdistributionTrees <- function( sim=output, it=1:3 ) {
  Time <- sim[,1]
  i.CPT <- sapply( 1:3, function(i){which( outputNames==paste0("CPT_t(",i,")"))} )
  i.CLT <- sapply( 1:3, function(i){which( outputNames==paste0("CLT_t(",i,")"))} )
  i.CBT <- sapply( 1:3, function(i){which( outputNames==paste0("CBT_t(",i,")"))} )
  i.CST <- sapply( 1:3, function(i){which( outputNames==paste0("CST_t(",i,")"))} )
  i.CRT <- sapply( 1:3, function(i){which( outputNames==paste0("CRT_t(",i,")"))} )
  for(t in it) {
    CPT   <- sim[,i.CPT[t]] ; CLT <- sim[,i.CLT[t]] ; CBT <- sim[,i.CBT[t]]
    CST   <- sim[,i.CST[t]] ; CRT <- sim[,i.CRT[t]]
    y.max <- max(CRT+CST+CBT+CLT+CPT)
    plot( Time, CRT+CST+CBT+CLT+CPT, ylim=c(0,y.max),
          main=paste0("Carbon in tree sp. ",it," (kg C m-2)"),
          xlab="", ylab="", type="l" )
    points( Time, CRT+CST+CBT+CLT, type="l", col="yellow" )
    points( Time, CRT+CST+CBT    , type="l", col="green"  )
    points( Time, CRT+CST        , type="l", col="red"    )
    points( Time, CRT            , type="l", col="blue"   )
    legend( "bottom", cex=0.7, lty=1, bg="white",
            col=c("black","yellow","green","red","blue"),
            legend=c("+ CPT: Fruits",
                     "+ CLT: Leaves",
                     "+ CBT: Branches",
                     "+ CST: Stems",
                     "   CRT: Roots" ) )
  }
}

#######################################################################
### 7. Function for extracting a site-specific MAP from a multi-site BC
#######################################################################
params.MAP.s <- function(s) {
  params_BC_MAP   <- scparMAP_BC * sc ; params        <- list_params[[s]]
  ip_BC_s         <- ip_BC_site [[s]] ; icol_pChain_s <- icol_pChain_site[[s]]
  params[ip_BC_s] <- params_BC_MAP[icol_pChain_s]
  return( params ) }

#######################################################################
### 8. Functions for running multiple sites
#######################################################################
fOutputTM <- function( parMatrix=parMAP, sfiles=sitesettings_filenames ) {

  nSites         <- length(sitesettings_filenames)
  idoy           <- which( outputNames=="doy" )
  names_vars     <- c( "Site",
    "harvDM_f_hay", "harvCPT_f", "harvCPT_2", "harvCST_f", "harvCST_3",
    "D_Nsoil"     , "D_Csoil"  , "D_C"      , "D_CT"     , "D_Csys"    )
  fOut           <- matrix( NA, nrow=nSites, ncol=length(names_vars) )
  colnames(fOut) <- names_vars
  fOut[,1]       <- 1:nSites
  
  ih_f_hay <- which( outputNames=="harvDM_f_hay" ) # kg C ha-1 y-1
  ihCPT_f  <- which( outputNames=="harvCPT_f"    ) # kg C m-2 d-1
  ihCPT_2  <- which( outputNames=="harvCPT_t(2)" ) # kg C m-2 d-1
  ihCST_f  <- which( outputNames=="harvCST_f"    ) # kg C m-2 d-1
  ihCST_3  <- which( outputNames=="harvCST_t(3)" ) # kg C m-2 d-1
  iNsoil_f <- which( outputNames=="Nsoil_f"      ) # kg N m-2
  iCsoil_f <- which( outputNames=="Csoil_f"      ) # kg C m-2 
  iC_f     <- which( outputNames=="C_f"          ) # kg C m-2
  iCT_f    <- which( outputNames=="CT_f"         ) # kg C m-2

  for (s in 1:nSites) {
    source(sitesettings_filenames[s] )
    params   <- parMatrix[,s]
    output.s <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                           calendar_prunT, calendar_thinT, NDAYS )
    
    c.DM  <- 1 / 0.47          # from 'kg C' to 'kg DM'
    c.hay <- 1e4 * 365 / NDAYS # from 'm-2 over simulation period' to 'ha-1 y-1'
    d365  <- which( output.s[,idoy]==365 )
    
    fOut[s,"harvDM_f_hay"] <- mean( output.s[d365,ih_f_hay] )
    fOut[s,"harvCPT_f"   ] <- sum ( output.s[    ,ihCPT_f ] ) * c.DM * c.hay
    fOut[s,"harvCPT_2"   ] <- sum ( output.s[    ,ihCPT_2 ] ) * c.DM * c.hay
    fOut[s,"harvCST_f"   ] <- sum ( output.s[    ,ihCST_f ] ) * c.DM * c.hay
    fOut[s,"harvCST_3"   ] <- sum ( output.s[    ,ihCST_3 ] ) * c.DM * c.hay
    
    D_output.s <- output.s[NDAYS,] - output.s[1,]
    fOut[s,"D_Nsoil"] <- D_output.s[iNsoil_f] * c.hay
    fOut[s,"D_Csoil"] <- D_output.s[iCsoil_f] * c.hay
    fOut[s,"D_C"    ] <- D_output.s[iC_f    ] * c.hay
    fOut[s,"D_CT"   ] <- D_output.s[iCT_f   ] * c.hay
    fOut[s,"D_Csys" ] <- fOut[s,"D_Csoil"] + fOut[s,"D_C"] + fOut[s,"D_CT"]
  }
  return( fOut )
}

barplotTM <- function( y, name=NULL, xtxt="" ) {
  colbars <- c( "gray40"    , "gray80"    , "gray60"    , "gray100"   ,
                "gray40"    , "gray80"    , "gray60"    , "gray100"   ,
                "gray80"    , "gray60"    ,
                "gray80"    , "gray60"    ,
                "gray40"    , "gray80"    , "gray60"    , "gray100"   ,
                "gray40"    , "gray80"    ,
                "firebrick4", "firebrick2", 
                "firebrick2", "firebrick3",
                "firebrick4", "firebrick2", "firebrick3", "firebrick1", 
                "firebrick4", "firebrick2", "firebrick3", "firebrick1",
                "firebrick2", "firebrick3" )
  barplot( as.matrix(y), main=name, col=colbars, beside=T, names.arg=xtxt )
}

# fYieldsTM <- function( parMatrix=parMAP, sfiles=sitesettings_filenames,
#                        WC=FALSE, list_w=weather_C.f, f=1 ) {
fYieldsTM <- function( parMatrix=parMAP, sfiles=sitesettings_filenames,
                       list_w=NULL, f=NULL ) {
    
  nSites            <- length(sitesettings_filenames)

  names_vars        <- c( "Site", "Y.obs", "Y.sim" )
  fYields           <- matrix( NA, nrow=nSites, ncol=3 )
  colnames(fYields) <- names_vars
  fYields[,"Site"]  <- 1:nSites
  
  iyear.model       <- which( outputNames=="year" )
  idoy.model        <- which( outputNames=="doy" )
  iH.model          <- which( outputNames=="harvDM_f_hay" )

  for (s in 1:nSites) {
    iH.data.s          <- which( data_name[[s]]=="harvDM_f_hay" )
    H.data.s           <- data_value[[s]] [iH.data.s]
    fYields[s,"Y.obs"] <- mean( H.data.s )
    
    nY.s               <- length( iH.data.s )
    year.data.s        <- data_year [[s]] [iH.data.s]
    doy.data.s         <- data_doy  [[s]] [iH.data.s]
    
    source(sitesettings_filenames[s] )
    params     <- parMatrix[,s]
    if( !is.null(list_w) ) matrix_weather <- read_listweather_CAF( list_w, f )
    output.s   <- run_model( params, matrix_weather,
                             calendar_fert, calendar_prunC,
                             calendar_prunT, calendar_thinT, NDAYS )
    iH.model.s <- sapply( 1:nY.s, function(i) {
      which( output.s[,iyear.model]==year.data.s[i] & 
             output.s[,idoy.model ]==doy.data.s [i] ) } )
    fYields[s,"Y.sim"] <- mean ( output.s[iH.model.s,iH.model] )
  }
  return( fYields )
}

plotYieldsTM <- function( y ) {
  nSites <- dim(y)[1]
  y.max  <- max(y[,c("Y.obs","Y.sim")])
  plot( y[,"Y.sim"], y[,"Y.obs"],
        main="Av. yield coffee\n(kg DM ha-1 y-1)",
        xlab="Sim. yield (kg ha-1)", ylab="",
        xlim=c(0,y.max), ylim=c(0,y.max),
        cex.main=1, type="n", asp=1 )
  colVector <- rep("",nSites)
  colVector[ which( y[,"Site"]<=18 ) ] <- "black"
  colVector[ which( y[,"Site"]>=19 ) ] <- "red"
  text( y[,"Y.sim"], y[,"Y.obs"],
        labels=y[,"Site"], cex=0.8, col=colVector )
  abline(0,1,lty=2)
  y.lm <- lm(y[,"Y.obs"]~y[,"Y.sim"])
  r2   <- signif( summary(y.lm)$r.squared, 2 )
  abline( y.lm$coefficients[1], y.lm$coefficients[2], col="blue" )
  legend( "bottomright",
          legend=c("y=x", paste("r2= ",as.character(r2)) ),
          col=c("black","blue"), lty=c(2,1), cex=0.7 )
}
