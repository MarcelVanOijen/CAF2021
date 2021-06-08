par( mfrow=c(3,1), mar=c(3,2,2,0) )

barplot( as.matrix(Balances[,"D_Csys"]),
         main="System C (kg ha-1 y-1)",
         beside=TRUE, names.arg="" )
barplot( as.matrix(Balances[,"D_Csoil"]),
         main="Soil C (kg ha-1 y-1)",
         beside=TRUE, names.arg="" )
barplot( as.matrix(Balances[,"D_Nsoil"]),
         main="Soil N (kg ha-1 y-1)",
         beside=TRUE, names.arg=1:nSites )
