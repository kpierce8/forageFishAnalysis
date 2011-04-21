##Eggs by Month time-series
par(par_reset)
#par(mfcol=c(1,2))
colramp<- heat.colors(5)
cbob<- color.gradient(c(1,0,0),c(0,1,0),c(0,0,1),nslices=12)
years<- sort(unique(ffsall$Month_))
ffsall$colcut <- cut(ffsall[,26], breaks=c(0,.2,.4,.6,.8,1),labels=c(1,2,3,4,5))

for(i in 1:length(years)){
yrdata<-ffsall[ffsall$Month_ == years[i],]
yrsmelt <- yrdata[yrdata[,6] == 1,]

dim(yrdata)
par(pty="s")

plot(yrdata[,11],yrdata[,12],pch=".",xlim=c(500000,1300000),ylim=c(600000,1400000),xlab="easting",ylab="northing")
points(yrdata$East_Coord,yrdata$North_Coord,col=cbob[yrdata$colcut],pch=16)
#points(yrsmelt$East_Coord,yrsmelt$North_Coord,col=3,pch=20)
text(1220000,1350000,paste("mos=",years[i]))
text(1220000,1300000,paste("N=",dim(yrdata)[1]))
title("Live Proportion Smelt Egg Observations \n color-coded by live proportion",sub="",cex=.5)
legend(650000,1380000,legend=c("0-.2",".2-.4",".4-.6",".6-.8",".8-1"),pch=20,col=cbob,cex=.75)
if(i %% 1 == 0) savePlot(filename=paste(pldir,"\\EggsByMonth",i,".jpg",sep=""),type="jpg")
}


