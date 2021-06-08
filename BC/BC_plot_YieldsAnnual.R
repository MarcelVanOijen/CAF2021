par( mfrow=c(1,1), mar=c(5,5,2,2) )

AllYields      <- do.call(rbind, Yields)
AllYields.data <- AllYields[,"Y.data"] ; AllYields.model <- AllYields[,"Y.MAP"]
plot( AllYields.model, AllYields.data, main="Yields coffee\n(kg DM ha-1 y-1)",
      xlab="Sim.", ylab="Obs.",
      cex.main=1, type="n" )
colVector <- rep("",nSites)
colVector[ which( AllYields[,"Site"]<=18 ) ] <- "black"
colVector[ which( AllYields[,"Site"]>=19 ) ] <- "red"
text( AllYields.model, AllYields.data,
      labels=AllYields[,"Site"], cex=0.8, col=colVector )
abline(0,1,lty=2)
AllYields.lm <- lm(AllYields[,"Y.data"]~AllYields[,"Y.MAP"])
AllYields.r2 <- signif( summary(AllYields.lm)$r.squared, 2 )
abline( AllYields.lm$coefficients[1], AllYields.lm$coefficients[2], col="blue" )
legend( "topleft",
        legend=c("y=x", paste("r2= ",as.character(AllYields.r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )
