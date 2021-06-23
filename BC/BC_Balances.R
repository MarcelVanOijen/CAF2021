source('initialisation/initialise_CAF2021_general.R')

iNsoil_f <- which( outputNames=="Nsoil_f") # kgN m-2
iCsoil_f <- which( outputNames=="Csoil_f") # kgC m-2 
iC_f     <- which( outputNames=="C_f" )    # kgC m-2
iCT_f    <- which( outputNames=="CT_f")    # kgC m-2

params_BC_MAP      <- scparMAP_BC  * sc

Balances           <- matrix( NA, nrow=nSites, ncol=6 )
colnames(Balances) <- c( "Site", "D_Nsoil", "D_Csoil", "D_C", "D_CT", "D_Csys" )
Balances[,1]       <- 1:nSites

for (s in 1:nSites) {
  params         <- list_params        [[s]] ; matrix_weather <- list_matrix_weather[[s]]
  calendar_fert  <- list_calendar_fert [[s]] ; calendar_prunC <- list_calendar_prunC[[s]] 
  calendar_prunT <- list_calendar_prunT[[s]] ; calendar_thinT <- list_calendar_thinT[[s]]  
  NDAYS          <- list_NDAYS         [[s]]       
  ip_BC_s        <- ip_BC_site         [[s]]
  icol_pChain_s  <- icol_pChain_site   [[s]]

# Calculate model output for the MAP parameter vector
  params[ip_BC_s] <- params_BC_MAP      [icol_pChain_s]
  outputMAP       <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                                calendar_prunT, calendar_thinT, NDAYS )
  fconv <- 1e4 * 365 / NDAYS # from 'm-2 over simulation period' to 'ha-1 y-1'
  D_outputMAP <- ( outputMAP[NDAYS,] - outputMAP[1,] ) * fconv
  Balances[s,"D_Nsoil"] <- D_outputMAP[iNsoil_f]
  Balances[s,"D_Csoil"] <- D_outputMAP[iCsoil_f]
  Balances[s,"D_C"    ] <- D_outputMAP[iC_f    ]
  Balances[s,"D_CT"   ] <- D_outputMAP[iCT_f   ]
  Balances[s,"D_Csys" ] <- Balances[s,"D_Csoil"] + Balances[s,"D_C"] + Balances[s,"D_CT"]
}
