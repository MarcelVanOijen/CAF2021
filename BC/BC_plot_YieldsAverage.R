par( mfrow=c(1,2), mar=c(5,2,2,1) )

AvYields.max <- max(AvYield[s.plot,c("Y.data","Y.MAP")])
plot( AvYield[s.plot,"Nfert"], AvYield[s.plot,"Y.data"],
      main="Av. yield coffee\n(kg DM ha-1 y-1)",
      xlab="N fert. (kg N ha-1 y-1)", ylab="Obs.",
      ylim=c(0,AvYields.max),
      cex.main=1, type="n" )
colVector <- rep("",length(s.plot))
colVector[ which( AvYield[s.plot,"Site"]<=18 ) ] <- "black"
colVector[ which( AvYield[s.plot,"Site"]>=19 ) ] <- "red"
text( AvYield[s.plot,"Nfert"], AvYield[s.plot,"Y.data"],
      labels=AvYield[s.plot,"Site"], cex=0.8, col=colVector )
AvYield.Nfert.lm <- lm(AvYield[s.plot,"Y.data"]~AvYield[s.plot,"Nfert"])
r2   <- signif( summary(AvYield.Nfert.lm)$r.squared, 2 )
abline( AvYield.Nfert.lm$coefficients[1], AvYield.Nfert.lm$coefficients[2],
        col="blue" )
legend( "bottomright",
        legend=c("y=x", paste("r2= ",as.character(r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )

AvYields.max   <- max(AvYield[s.plot,c("Y.data","Y.MAP")])
plot( AvYield[s.plot,"Y.MAP"], AvYield[s.plot,"Y.data"],
      main="Av. yield coffee\n(kg DM ha-1 y-1)",
      xlab="Sim. yield (kg ha-1)", ylab="",
      xlim=c(0,AvYields.max), ylim=c(0,AvYields.max),
      cex.main=1, type="n" )
colVector <- rep("",length(s.plot))
colVector[ which( AvYield[s.plot,"Site"]<=18 ) ] <- "black"
colVector[ which( AvYield[s.plot,"Site"]>=19 ) ] <- "red"
text( AvYield[s.plot,"Y.MAP"], AvYield[s.plot,"Y.data"],
      labels=AvYield[s.plot,"Site"], cex=0.8, col=colVector )
abline(0,1,lty=2)
AvYield.lm <- lm(AvYield[s.plot,"Y.data"]~AvYield[s.plot,"Y.MAP"])
r2   <- signif( summary(AvYield.lm)$r.squared, 2 )
abline( AvYield.lm$coefficients[1], AvYield.lm$coefficients[2],
        col="blue" )
legend( "bottomright",
        legend=c("y=x", paste("r2= ",as.character(r2)) ),
        col=c("black","blue"), lty=c(2,1), cex=0.7 )
