par( mfrow=c(3,1), mar=c(3,2,2,0) )

colbars <- c( "gray40"    , "gray80"    , "gray60"    , "gray100"   ,
              "gray40"    , "gray80"    , "gray60"    , "gray100"   ,
              "gray80"    , "gray60"    ,
              "gray80"    , "gray60"    ,
              "gray40"    , "gray80"    , "gray60"    , "gray100"   ,
              "gray40"    , "gray80"    ,
              "firebrick4", "firebrick2", 
              "firebrick2", "firebrick3",
              "firebrick4", "firebrick2", "firebrick3", "firebrick1", 
              "firebrick4", "firebrick2", "firebrick3", "firebrick1",
              "firebrick2", "firebrick3" )

barplot( as.matrix(Balances[,"D_Csys"]),
         main=paste( "Turrialba                  ",
                     "SYSTEM C (kg ha-1 y-1)",
                     "                   Masatepe" ),
         col=colbars, beside=TRUE, names.arg="" )
barplot( as.matrix(Balances[,"D_Csoil"]),
         main="Soil C (kg ha-1 y-1)",
         col=colbars, beside=TRUE, names.arg="" )
barplot( as.matrix(Balances[,"D_Nsoil"]),
         main="Soil N (kg ha-1 y-1)",
         col=colbars, beside=TRUE, names.arg=1:nSites )
