shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\lonlatplots", sep=""))
script <- "\\lonlatplots"

ffslatcut <- cut(ffsall[,13], breaks=10)
ffsloncut <- cut(ffsall[,14], breaks=10)

slkk <- data.frame(ffsall[,c(6,7,15,17,13,14)],ffslatcut,ffsloncut)

tapply(slkk$Smelt_Ind,slkk[,c(7,8)],sum)
tapply(slkk$Smelt_Ind,slkk[,c(7,8)],length)

lonlatSS <- tapply(slkk$Smelt_Ind,slkk[,c(7,8)],sum)/ tapply(slkk$Smelt_Ind,slkk[,c(7,8)],length)

lonlatSS <- sig3(lonlatSS)

image(names(lonlatSS),rev(rownames(lonlatSS)),lonlatSS)

image(t(lonlatSS),col=heat.colors(6),xlab="lat",ylab="lon",main="Smelt")
#axis(1,at=c(0,seq(2:12)/11),labels=seq(1:12))
savePlot(paste(pldir,script,"\\SSlonlat.jpg",sep=""),type="jpg")

####################################

tapply(slkk$Sand_Lance_Ind,slkk[,c(7,8)],sum)
tapply(slkk$Sand_Lance_Ind,slkk[,c(7,8)],length)

lonlatSL <- tapply(slkk$Sand_Lance_Ind,slkk[,c(7,8)],sum)/ tapply(slkk$Sand_Lance_Ind,slkk[,c(7,8)],length)

lonlatSL <- data.frame(sig3(lonlatSL))
names(lonlatSL) <- names(lonlatSS)
#image(names(lonlatSL),rev(rownames(lonlatSL)),lonlatSL)

image(t(lonlatSL),col=heat.colors(6),xlab="lat",ylab="lon",main="Lance")
#axis(1,at=c(0,seq(2:12)/11),labels=seq(1:12))

savePlot(paste(pldir,script,"\\SLlonlat.jpg",sep=""),type="jpg")
 #############################

tapply(slkk$Smelt_Ind,slkk[,c(7,8)],sum)
tapply(slkk$Sand_Lance_Ind,slkk[,c(7,8)],length)
lonlatAll <- tapply(slkk$Sand_Lance_Ind,slkk[,c(7,8)],length)
lonlatAll <- sig3(lonlatAll)

image(t(lonlatAll),col=heat.colors(6),xlab="lat",ylab="lon",main="Samples")
#axis(1,at=c(0,seq(2:12)/11),labels=seq(1:12))
savePlot(paste(pldir,script,"\\Sampleslonlat.jpg",sep=""),type="jpg")