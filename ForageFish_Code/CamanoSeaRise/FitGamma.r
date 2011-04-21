shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\FitGamma", sep=""))
script <- "\\FitGamma"



sq1 <- "TRANSFORM Avg(SmeltEggs.Elev) AS AvgOfElev SELECT SmeltEggs.BeachNo FROM SmeltEggs GROUP BY SmeltEggs.BeachNo PIVOT SmeltEggs.TransLetter;"

sq2 <- "TRANSFORM Avg(SmeltEggs.EstNeggs) AS AvgOfEstNeggs SELECT SmeltEggs.BeachNo FROM SmeltEggs GROUP BY SmeltEggs.BeachNo PIVOT SmeltEggs.TransLetter;"


ffconn<- odbcConnectAccess("e:\\foragefish\\FF_Sample_Assessment.mdb")


elevdata <-  sqlQuery(ffconn,sq1)
eggdata <-  sqlQuery(ffconn,sq2)
beaches <- sqlFetch(ffconn,"BeachLocation")
beaches <- beaches[-7,]

ffconn2<- odbcConnectAccess("C:\\data\\ProjectsLocal\\FORAGE_FISH\\Camano\\Camano.mdb")
camano <- sqlFetch(ffconn2,"camano")


elevdata0 <- as.matrix(elevdata[,c(1,4,5,3,2)])
eggdata0 <- as.matrix(eggdata[,c(4,5,3,2)])

egg.counts<-apply(eggdata0,1,sum)
eggmin <- 50

elevdata1 <- elevdata0[egg.counts > eggmin,]
eggdata1 <- eggdata0[egg.counts > eggmin,]

#Original row 26 fails to converge when fitting Gamma with nlminb
elevdata1 <- elevdata1[-16,]
eggdata1 <- eggdata1[-16,]

eget <- match(beaches$BeachNo, camano$STATION)
beaches <- data.frame(beaches, MHHW= camano[eget,"MHH1_ELEV"])

#fix MHHW below
bget <- match(as.vector(elevdata1[,1]),as.vector(beaches$BeachNo))
beach2 <- beaches[bget,]

elevsq <- elevdata1
for(i in 1:nrow(elevdata1)){
slop = 1.25
elevsq[i,] <- ((beach2$MHHW[i]+slop)-elevdata1[i,])/(beach2$MHHW[i]+slop)
}
elevq <- elevsq

eggq <- eggdata1
for(i in 1:nrow(elevdata1)){
eggq[i,] <- cumsum(eggdata1[i,])/(sum(eggdata1[i,])*1)
}

eggq2 <- eggq[!is.na(eggq[,4]),]
#eggq2 <- eggq2[c(-2,-4,-22),]

elevq2 <- elevq[!is.na(eggq[,4]),]
#elevq2 <- elevq2[c(-2,-4,-22),]




# RUN BY HAND, need a try message or something
###############################################

i<- 1   # 4 and 14 seem to not fit

modpar <- matrix(0,nrow(eggq2),2)
print(i)
gseq <- c(1:3,5:13,15)
for(i in 1:length(gseq)){

eggd<- eggq2[gseq[i],]
elevd <- elevq2[gseq[i],-1]
fmin <- function(par){sum((eggd- pgamma(elevd,par[1],par[2]))^2)}
minmod <- nlminb(c(1,1),fmin, control = list(trace=TRUE))
modpar[gseq[i],] <- minmod$par
i <- i +1
}
################################################
#########CUMULATIVE ANALYSIS
eggqi <- eggdata1
for(i in 1:nrow(elevdata1)){
eggqi[i,] <- eggdata1[i,]/(sum(eggdata1[i,])*1)
}

eggqi2 <- eggqi[!is.na(eggqi[,4]),]
#eggqi2 <- eggqi2[c(-2,-4,-22),]

elevq2<- elevq2[,-1]
allelevq<-elevq2[order(elevq2)]
alleggq<-eggqi2[order(elevq2)]

elevs.mean <-tapply(allelevq,as.factor(allelevq),mean)
eggs.sum<-tapply(alleggq,as.factor(allelevq),sum)
eggs.sd<-tapply(alleggq,as.factor(allelevq),sd)
eggs.csum <- cumsum(eggs.sum)/sum(eggs.sum)
elev.wt<-table(allelevq)
plot(elevs.mean,eggs.csum)
plot(elevs.mean,eggs.sum/elev.wt)

fmin <- function(par){sum(abs(eggs.csum- pgamma(elevs.mean,par[1],par[2]))^1)}
minmod <- nlminb(c(1,1),fmin)
bob<- rgamma(10000,minmod$par[1],minmod$par[2])
par(par_reset)
bob2<-density(bob)
plot(bob2,xlim=c(0,1))
points(elevs.mean,eggs.sum/elev.wt,pch=20,cex=sqrt(elev.wt))
savePlot(paste(pldir,script,"\\AllDensity.pdf",sep=""),type="pdf")
plot(elevs.mean,eggs.csum,pch=20,cex=sqrt(elev.wt),xlim=c(0,1))
points(elevs.mean,eggs.csum+eggs.sd,pch=25,bg=2)
points(elevs.mean,eggs.csum-eggs.sd,pch=24,bg=2)
lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),minmod$par[1],minmod$par[2]))
savePlot(paste(pldir,script,"\\AllCumul.pdf",sep=""),type="pdf")

write.csv(modpar,"f:/foragefish/modparSQ.csv")
###############################################################################
###Plot all the distributions together###
par(par_reset)

bob<- rgamma(1000,modpar[10,1],modpar[10,2])
bob2<-density(bob)
plot(bob2,xlim=c(0,.6),ylim=c(0,40),col=2)

#par(mfrow=c(4,3))
for(rowv in 2:dim(eggq2)[1]){
bob<- rgamma(1000,modpar[rowv,1],modpar[rowv,2])
pcol <- round(modpar[rowv,2]/modpar[rowv,1])
bob2<-density(bob)
lines(bob2,col=pcol)
rowv <- rowv + 1
print(paste(rowv,pcol))
}

savePlot(paste(pldir,script,"\\GammaPlot.pdf",sep=""),type="pdf")
##############################################################################
options(graphics.record=TRUE)
par(mfrow=c(3,2))
for(rowv in 1:dim(eggq2)[1]){
#rowv <- 1
pcol <- round(modpar[rowv,2]/modpar[rowv,1])
plot(elevq2[rowv,],eggq2[rowv,],xlim=c(0,1),ylim=c(0,1))
lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),modpar[rowv,1],modpar[rowv,2]),col=pcol)
text(.9,.7,rowv)
lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),1,pcol))
if(rowv %% 6 == 0) savePlot(filename=paste(pldir,script,"\\GammaFit",rowv,".pdf",sep=""),type="pdf")

}
savePlot(filename=paste(pldir,script,"\\GammaFit",rowv,".pdf",sep=""),type="pdf")

modpar <-  data.frame(modpar,ratio = modpar[,2]/modpar[,1])


habloss <- matrix(0,nrow(elevdata1),4)
habloss[,1] <- pgamma(.5/elevdata1[,1],modpar[,1],modpar[,2])
habloss[,2] <- pgamma(1/elevdata1[,1],modpar[,1],modpar[,2])
habloss[,3] <- pgamma(1.5/elevdata1[,1],modpar[,1],modpar[,2])
habloss[,4] <- pgamma(2/elevdata1[,1],modpar[,1],modpar[,2])

boxplot(habloss[,1],habloss[,2],habloss[,3],habloss[,4],col=c(3,2),main="Hab Loss",names=c(0.5,1,1.5,2))

savePlot(filename=paste(pldir,script,"\\HabLoss.pdf",sep=""),type="pdf")
                                                                   #1 foot higher scenario
habloss2 <- matrix(0,nrow(elevdata1),4)
habloss2[,1] <- pgamma(.5/beach2$MHHW,modpar[,1],modpar[,2])
habloss2[,2] <- pgamma(1/beach2$MHHW,modpar[,1],modpar[,2])
habloss2[,3] <- pgamma(1.5/beach2$MHHW,modpar[,1],modpar[,2])
habloss2[,4] <- pgamma(2/beach2$MHHW,modpar[,1],modpar[,2])

boxplot(habloss2[,1],habloss2[,2],habloss2[,3],habloss2[,4],col=c(3,2),main="Hab Loss plus 1",names=c(0.5,1,1.5,2))
savePlot(filename=paste(pldir,script,"\\HabLoss_plus1.pdf",sep=""),type="pdf")

#Plot command for a polygon up to a certain percentage
polygon(c(seq(0,.30,by=.01),.30),c(pgamma(seq(0,.30,by=.01),1,pcol),0),col=5)