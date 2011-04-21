##Run fitgamma to initialize variables
shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\FitGamma", sep=""))
script <- "\\FitGamma"


par(mfrow=c(1,1))
#Plot 1
#rowv <- 6
#pcol <- round(modpar[rowv,2]/modpar[rowv,1])
#plot(elevq2[rowv,],eggq2[rowv,],xlim=c(0,1),pch=2,ylim=c(0,1),xlab="Beach Transect (High to Low)", ylab="Cumulative Egg Proportion")
#lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),modpar[rowv,1],modpar[rowv,2]),col=pcol,lwd=2)
##text(.9,.7,rowv)
#lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),1,pcol))

plot(elevs.mean,eggs.csum,pch=20,cex=sqrt(elev.wt),xlim=c(0,1),ylim=c(0,1),xlab="Beach Transect (High to Low)", ylab="Cumulative Egg Proportion")
##SD deviations omitted for B & W
#points(elevs.mean,eggs.csum+eggs.sd,pch=25,bg=2)
#points(elevs.mean,eggs.csum-eggs.sd,pch=24,bg=2)
lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),minmod$par[1],minmod$par[2]))

savePlot(paste(pldir,script,"\\PosterPlot1.emf",sep=""),type="emf")



##Plot2
                                                                   #1 foot higher scenario
habloss2 <- matrix(0,nrow(elevdata1),4)
habloss2[,1] <- pgamma(.5/(beach2$MHHW+1.5),modpar[,1],modpar[,2])
habloss2[,2] <- pgamma(1/(beach2$MHHW+1.5),modpar[,1],modpar[,2])
habloss2[,3] <- pgamma(1.5/(beach2$MHHW+1.5),modpar[,1],modpar[,2])
habloss2[,4] <- pgamma(2/(beach2$MHHW+1.5),modpar[,1],modpar[,2])

boxplot(habloss2[,1],habloss2[,2],habloss2[,3],habloss2[,4],main="Spawning Habitat Shift above MHHW + 1.5",names=c(0.5,1,1.5,2),xlab="Modeled Sea Level Rise Scenarios (ft)",ylab="Proportion Habitat Shifted")

savePlot(paste(pldir,script,"\\PosterPlot2.pdf",sep=""),type="pdf")
##Plot 3
plot(elevs.mean,eggs.csum,pch=20,cex=sqrt(elev.wt),xlim=c(0,.8),ylim=c(0,1),xlab="Beach Transect (High to Low)", ylab="Cumulative Egg Proportion")
#polygon(c(seq(0,.20,by=.01),.20),c(pgamma(seq(0,.20,by=.01),minmod$par[1],minmod$par[2]),0),col=rgb(0,.75,0,.25))
#polygon(c(pgamma(seq(0,.20,by=.01),minmod$par[1],minmod$par[2]),0),c(rep(pgamma(.20,minmod$par[1],minmod$par[2]),22)),col=rgb(0,.75,0,.25))

points(elevs.mean,eggs.csum,pch=20,cex=sqrt(elev.wt))
#points(elevs.mean,eggs.csum+eggs.sd,pch=25,bg=2)
#points(elevs.mean,eggs.csum-eggs.sd,pch=24,bg=2)
lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),minmod$par[1],minmod$par[2]))
pend<-  c(pgamma(.20,minmod$par[1],minmod$par[2]))
lines(c(.2,.2),c(0,pend),lty=3)
lines(c(0,.2),c(pend,pend),lty=3)
#pcol <- round(modpar[rowv,2]/modpar[rowv,1])
#plot(elevq2[rowv,],eggq2[rowv,],pch=2,xlim=c(0,1),ylim=c(0,1),xlab="Beach Transect (High to Low)",ylab="Cumulative Egg Proportion")
#lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),modpar[rowv,1],modpar[rowv,2]),col=pcol,lwd=2)
##text(.9,.7,rowv)
#title("Cumulative Spawning Loss with Truncation of Upbeach Habitat")
##lines(seq(0,1,by=.01),pgamma(seq(0,1,by=.01),1,pcol))
#polygon(c(seq(0,.30,by=.01),.30),c(pgamma(seq(0,.30,by=.01),modpar[rowv,1],modpar[rowv,2]),0),col=5)
#points(elevq2[rowv,],eggq2[rowv,],pch=2)
savePlot(paste(pldir,script,"\\PosterPlot3.pdf",sep=""),type="pdf")
##Plot4

bob<- rgamma(1000,modpar[10,1],modpar[10,2])
bob2<-density(bob)
#plot(bob2,xlim=c(0,1),ylim=c(0,40),col=1,main="Modeled Distribution of Egg Density",xlab="Beach Transect (High to Low)" )
plot(bob2,xlim=c(0,1),ylim=c(0,40),col=1, main="", xlab="", ylab="")

for(rowv in 2:length(gseq)){
bob<- rgamma(1000,modpar[gseq[rowv],1],modpar[gseq[rowv],2])
pcol <- round(modpar[gseq[rowv],2]/modpar[gseq[rowv],1])
bob2<-density(bob)
lines(bob2,col=1)
#rowv <- rowv + 1
print(paste(gseq[rowv],pcol))
}
savePlot(paste(pldir,script,"\\PosterPlot4.pdf",sep=""),type="pdf")

