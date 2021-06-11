## set_CAF2021_Masatepe_trees_ILSG.R ##
## MvO, 2021-06-03

params <- set_par( c("HMAX(1)","TREEDENS0(1)","KNFIX(1)"),
                   c( 8       , 0.0333       , 0.05     ) )
params <- set_par( c(          "TREEDENS0(3)","KNFIX(3)"),
                   c(           0.0333       , 0        ) )

calendar_prunT[ 1,  1, ] <- c( 2003, 158, 0.1  )
calendar_prunT[ 1,  2, ] <- c( 2004,  49, 0.03 )
calendar_prunT[ 1,  3, ] <- c( 2004, 175, 0.03 )
calendar_prunT[ 1,  4, ] <- c( 2004, 320, 0.03 )
calendar_prunT[ 1,  5, ] <- c( 2005, 220, 0.1  )
calendar_prunT[ 1,  6, ] <- c( 2006, 220, 0.1  )
calendar_prunT[ 1,  7, ] <- c( 2007, 220, 0.1  )
calendar_prunT[ 1,  8, ] <- c( 2008, 220, 0.1  )
calendar_prunT[ 1,  9, ] <- c( 2010, 220, 0.1  )
calendar_prunT[ 1, 10, ] <- c( 2011, 220, 0.1  )
calendar_prunT[ 1, 11, ] <- c( 2012, 250, 0.1  )
calendar_prunT[ 1, 12, ] <- c( 2013, 226, 0.1  )
calendar_prunT[ 3,   , ] <- calendar_prunT[ 1, , ]

calendar_thinT[ 1,  1, ] <- c( 2008, 124, 0.50 )
calendar_thinT[ 3,   , ] <- calendar_thinT[ 1, , ]
