source("initialisation/initialise_CAF2021_Turrialba_15_CE_AC.R")
params[names_params=="TREEDENS0(1)"] <- 0.04
params[names_params=="TREEDENS0(2)"] <- 0.02
params[names_params=="TREEDENS0(3)"] <- 0.01
params[names_params=="LUEMAX(1)"]    <- 0.0004
params[names_params=="LUEMAX(2)"]    <- 0.0004
params[names_params=="LUEMAX(3)"]    <- 0.0012
params[names_params=="SLAT(1)"]      <- 20
params[names_params=="SLAT(2)"]      <- 20
params[names_params=="SLAT(3)"]      <- 20

calendar_prunT[ 2, , ] <- calendar_prunT[ 1, , ]
calendar_thinT[ 2, , ] <- calendar_thinT[ 1, , ]

cbind(names_params,params)

#####################

output <- run_model()

output_short <- signif( output, 5 )
icols        <- c(1:6,NOUT+(-1:0))
rbind( outputNames[icols],
       head(output_short[,icols]),
       rep("...",length(icols)),
       tail(output_short[,icols],n= 2,keepnums=F) )

finalvalues <- tail( output, 1 )
cbind( outputNames, outputUnits, t(finalvalues) )

plot_output( vars=c("Ac(1)","Ac(2)","Ac(3)","Ac(4)","Ac(5)","Ac(6)") )
plot_output( vars=c("At(1)","At(2)","At(3)") )
plot_output( vars=c("treedens_t(1)","treedens_t(2)","treedens_t(3)") )
plot_output( vars=c("h_t(1)","h_t(2)","h_t(3)") )
plot_output( vars=c("SAT_t(1)","SAT_t(2)","SAT_t(3)") )

## Variables around thinning

selecti.t <- function(v){ sapply( 1:3, function(i) {
  which( outputNames==paste0(v,"(",i,")" ) ) } ) }

i.h_t        <- selecti.t("h_t")
i.CST_t      <- selecti.t("CST_t")
i.SAT_t      <- selecti.t("SAT_t")
i.treedens_t <- selecti.t("treedens_t")
i.CAtree_t   <- selecti.t("CAtree_t")

i_year    <- which( outputNames=="year" ) ; i_doy    <- which( outputNames=="doy" )
year_firstthin <- calendar_thinT[ , 1, 1 ]    ; doy_firstthin <- calendar_thinT[ , 1, 2 ]
day_thin  <- which( (output[,i_year]==year_firstthin[1]) &
                    (output[,i_doy ]== doy_firstthin[1]), )

signif( output[ day_thin+(-1:2), c(i.treedens_t,i.CST_t,i.h_t) ], 3 )
signif( output[ day_thin+(-1:2), c(i.treedens_t,i.SAT_t) ], 3 )

signif( output[ 4288+(-3:3), c(i.treedens_t,i.CST_t   ,i.h_t  ) ], 4 )
signif( output[ 4288+(-3:3), c(i.treedens_t,i.CAtree_t,i.SAT_t) ], 4 )

