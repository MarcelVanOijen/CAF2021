######################################################################################################### 
 
guateCol<-"steelblue1"#LB
costaCol<-"indianred"#LB

#map
# setwd("C:/Users/Lucy/OneDrive - University of Greenwich/Lucie/NRI/Projects/Coffee/model")

library(maptools)
library(maps)
##load and prepare data (data downloaded from: gadm.org/download):
costa<-readRDS("gadm36_CRI_0_sp.rds")
guate<-readRDS("gadm36_GTM_0_sp.rds")
nica<-readRDS("gadm36_NIC_0_sp.rds")
hon<-readRDS("gadm36_HND_0_sp.rds")
pan<-readRDS("gadm36_PAN_0_sp.rds")
mex<-readRDS("gadm36_MEX_0_sp.rds")
coord<-read.delim("coordinates_farms.txt")

pdf("mapCentralAmerica.pdf",width=7,height=5.5)
#windows(7,5.5)
par(mar=c(0,0,0,0)+0.1)
plot(mex,lwd=1,border="grey20",xlim=c(-90,-82),ylim=c(8,18))
plot(nica,add=T,lwd=1,border="grey20")
plot(hon,add=T,lwd=1,border="grey20")
plot(pan,add=T,lwd=1,border="grey20")
plot(costa,add=T,lwd=1,border="grey20",col=costaCol)
plot(guate,add=T,lwd=1,border="grey20",col=guateCol)

text(guate@polygons[[1]]@labpt[1],guate@polygons[[1]]@labpt[2],"GUATEMALA",font=2,col="black")
text(costa@polygons[[1]]@labpt[1]-0.4,costa@polygons[[1]]@labpt[2]+0.5,"COSTA RICA",font=2,col="black")
text(pan@polygons[[1]]@labpt[1]-1,pan@polygons[[1]]@labpt[2],pan@data$NAME_0)
text(nica@polygons[[1]]@labpt[1],nica@polygons[[1]]@labpt[2],nica@data$NAME_0)
text(hon@polygons[[1]]@labpt[1],hon@polygons[[1]]@labpt[2],hon@data$NAME_0)
text(-92,16.7,mex@data$NAME_0)

points(coord$Longitude,coord$Latitude,pch=19,cex=0.7)
legend("bottomleft","sampled farms",pch=19,bty="n")
dev.off()

######################################################################################################### 

