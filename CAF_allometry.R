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
  
  # Masatepe default settings
  KH_IL <- 6   ; KHEXP_IL <- 0.35 # Inga.l.
  KH_SG <- 3.9 ; KHEXP_SG <- 0.4  # S.g.
  x   <- seq( 0, 40, 0.1 )
  h_IL <- fh( x, KH_IL, KHEXP_IL )
  h_SG <- fh( x, KH_SG, KHEXP_SG )
  par(mfrow=c(1,1))
  plot  ( x, h_IL, type='l' )
  points( x, h_SG, type='l', col="red" )
  legend( "bottomright", c("IL","SG"),
          lty=1, col=c("black","red") )
  
  # Height(CSpertree) = a * CSpertree ^ b
  # IL estimates: Height(3)=8, Height(20)=15
  # SG estimates: Height(3)=7, Height(20)=25
  b.IL <- ( log(15)-log(8) ) / ( log(20)-log(3) ) ; b.IL # KHEXP(IL) = 0.33
  b.SG <- ( log(25)-log(7) ) / ( log(20)-log(3) ) ; b.SG # KHEXP(SG) = 0.67
  a.IL <- exp( log(15) - b.IL * log(20) )         ; a.IL # KH   (IL) = 5.56
  a.SG <- exp( log(25) - b.SG * log(20) )         ; a.SG # KH   (SG) = 3.35
  # Verify by plotting:
  # KH_IL <- a.IL ; KHEXP_IL <- b.IL # Inga.l.
  # KH_SG <- a.SG ; KHEXP_SG <- b.SG # S.g.
  KH_IL <- 5 ; KHEXP_IL <- 0.3 # Inga.l.
  KH_SG <- 3 ; KHEXP_SG <- 0.7 # S.g.
  x   <- seq( 0, 20, 0.1 )
  h_IL <- fh( x, KH_IL, KHEXP_IL )
  h_SG <- fh( x, KH_SG, KHEXP_SG )
  par(mfrow=c(1,1))
  plot  ( x, h_IL, type='l', ylim=c(0,max(h_IL,h_SG)) )
  points( x, h_SG, type='l', col="red" )
  legend( "bottomright", c("IL","SG"),
          lty=1, col=c("black","red") )
  