fLUEt <- function(temp,toptt=30,ttolt=9) {exp( -0.5*((temp-toptt)/ttolt)^2. )}
temps <- 0:40

par( mfrow=c(1,1) )
plot  ( temps, fLUEt( temps, 30,  9 ), type="l" )
points( temps, fLUEt( temps, 25,  8 ), type="l", col="red" )