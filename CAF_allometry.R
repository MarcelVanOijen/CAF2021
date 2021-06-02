  fh <- function( CSpertree_t, KH=1, KHEXP=1 ) {
                  KH  * (CSpertree_t^KHEXP ) }

# MAP Turrialba from BC carried out in 2021-04
  KH_E <- 8.6 ; KHEXP_E <- 0.28 # Erythrina
  KH_T <- 3.9 ; KHEXP_T <- 0.41 # Terminalia
  KH_C <- 3.4 ; KHEXP_C <- 0.39 # Chrloroleucon

  x   <- seq( 0, 20, 0.1 )
  h_E <- fh( x, KH_E, KHEXP_E )
  h_T <- fh( x, KH_T, KHEXP_T )
  h_C <- fh( x, KH_C, KHEXP_C )

  par(mfrow=c(1,1))

  plot  ( x, h_E, type='l' )
  points( x, h_T, type='l', col="red" )
  points( x, h_C, type='l', col="blue" )
  legend( "bottomright", c("E","T","C"),
          lty=1, col=c("black","red","blue") )

  h1 <- fh( x, 10, 0.1 )
  h2 <- fh( x,  3, 0.7 )
  h3 <- fh( x,  1, 1   )
  ymax <- max( h1, h2, h3 )
  plot  ( x, h1, type="l", ylim=c(0,ymax) )
  points( x, h2, type="l", col="red" )
  points( x, h3, type="l", col="blue" )
  