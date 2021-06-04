## set_CAF2021_Turrialba_trees_E_MCMOBO.R ##
## MvO, 2021-06-04

params <- set_par( "TREEDENS0(1)", 0.07 )

calendar_prunT[ 1, 1   ,   ] <- c( 2001, 266, 0.4 )
calendar_prunT[ 1, 2:33, 1 ] <- rep( 2002:2017, each=2 )
calendar_prunT[ 1, 2:33, 2 ] <- c( 140, 350 )
calendar_prunT[ 1, 2:33, 3 ] <- 0.6

calendar_thinT[ 1, 1   ,   ] <- c( 2007, 124, 0.0075 )
calendar_thinT[ 1, 2   ,   ] <- c( 2008, 124, 0.49   )
calendar_thinT[ 1, 3   ,   ] <- c( 2013, 160, 0.029  )
