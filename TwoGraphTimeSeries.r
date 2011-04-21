##Year by Season time-series
par(par_reset)

shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\TwoGraphTime", sep=""))
script <- "\\TwoGraphTime"



colramp<- heat.colors(5)
cbob<- color.gradient(c(1,0,0),c(0,1,0),c(0,0,1),nslices=5)
years<- sort(unique(ffsall$Year_))
for(i in 29:34){


windows(width=11, height=8,xpinch=144,ypinch=144)
par(mfcol=c(1,2))
par(pty="s")
plot(ffsall[,11],ffsall[,12],pch=".",xlim=c(500000,1300000),ylim=c(600000,1400000),xlab="easting",ylab="northing")
yrdata<-ffsall[ffsall$Year_ == years[i],]
yrsmelt <- yrdata[yrdata[,6] == 1,]
dim(yrdata)
points(yrdata$East_Coord,yrdata$North_Coord,col=cbob[yrdata$moscut],pch=16)
#points(yrsmelt$East_Coord,yrsmelt$North_Coord,col=3,pch=20)
text(1220000,1350000,years[i])
title("Locations of Smelt Egg Observations \n color-coded by survey season",sub="",cex=.5)
legend(650000,1380000,legend=c("Summer","Summerish","Equinox","Winterish","Winter"),pch=20,col=cbob,cex=.75)

plot(ffsall[,11],ffsall[,12],pch=".",xlim=c(500000,1300000),ylim=c(600000,1400000),xlab="easting",ylab="northing")
points(yrdata$East_Coord,yrdata$North_Coord,col=cbob[yrdata$colcut],pch=16)
#points(yrsmelt$East_Coord,yrsmelt$North_Coord,col=3,pch=20)
text(1220000,1350000,years[i])
title("Live Proportion Smelt Egg Observations \n color-coded by live proportion",sub="",cex=.5)
legend(650000,1380000,legend=c("0-.2",".2-.4",".4-.6",".6-.8",".8-1"),pch=20,col=cbob,cex=.75)
if(i %% 1 == 0) savePlot(filename=paste(pldir,script,"\\TwoView",i,".jpg",sep=""),type="jpg")

}


plot(ffsall[,16],ffsall[,26],pch=".")