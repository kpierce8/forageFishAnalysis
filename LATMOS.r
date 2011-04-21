shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\LatMosplots", sep=""))


ffslatcut <- cut(ffsall[,13], breaks=10)
ffsloncut <- cut(ffsall[,14], breaks=10)

slkk <- data.frame(ffsall[,c(6,7,15,17,13,14)],ffslatcut,ffsloncut)

tapply(slkk$Smelt_Ind,slkk[,c(7,3)],sum)
tapply(slkk$Smelt_Ind,slkk[,c(7,3)],length)

latmosSS <- tapply(slkk$Smelt_Ind,slkk[,c(7,3)],sum)/ tapply(slkk$Smelt_Ind,slkk[,c(7,3)],length)

latmosSS <- sig3(latmosSS)

image(names(latmosSS),rev(rownames(latmosSS)),latmosSS)

image(t(latmosSS),col=heat.colors(6),xaxt= "n",xlab="Surf Smelt",ylab="Lat")
axis(1,at=c(0,seq(2:12)/11),labels=seq(1:12))

savePlot(paste(pldir,script,"\\SSlatmos.jpg",sep=""),type="jpg")

####################################

tapply(slkk$Sand_Lance_Ind,slkk[,c(7,3)],sum)
tapply(slkk$Sand_Lance_Ind,slkk[,c(7,3)],length)

latmosSL <- tapply(slkk$Sand_Lance_Ind,slkk[,c(7,3)],sum)/ tapply(slkk$Sand_Lance_Ind,slkk[,c(7,3)],length)

latmosSL <- data.frame(sig3(latmosSL))
names(latmosSL) <- names(latmosSS)
#image(names(latmosSL),rev(rownames(latmosSL)),latmosSL)

image(t(latmosSL),col=heat.colors(6),xaxt= "n",xlab="Sand Lance",ylab="Lat")
axis(1,at=c(0,seq(2:12)/11),labels=seq(1:12))

savePlot(paste(pldir,script,"\\SLlatmos.jpg",sep=""),type="jpg")
 #############################

tapply(slkk$Smelt_Ind,slkk[,c(7,3)],sum)
tapply(slkk$Sand_Lance_Ind,slkk[,c(7,3)],length)

latmosAll <- tapply(slkk$Sand_Lance_Ind,slkk[,c(7,3)],length)

latmosAll <- sig3(latmosAll)



image(t(latmosAll),col=heat.colors(6),xaxt= "n",xlab="Samples",ylab="Lat")
axis(1,at=c(0,seq(2:12)/11),labels=seq(1:12))
savePlot(paste(pldir,script,"\\Sampleslatmos.jpg",sep=""),type="jpg")