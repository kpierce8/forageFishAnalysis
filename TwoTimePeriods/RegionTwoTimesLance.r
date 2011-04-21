##This analysis is based on the four arbitrary regions and their perceived 
#temporal differences between the 2 time periods
shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\Reg2timesL", sep=""))
script <- "\\Reg2timesL"

library(RODBC)
ddir <- c("e")

FFS <- odbcConnectAccess(paste(ddir,":/ForageFish/Pierce/myForageFish.mdb",sep=""))

ffsall <- sqlFetch(FFS, "FFS_all")
szline <- sqlFetch(FFS, "szline")


ffsnoNA <- ffsall[!is.na(ffsall$ZoneFactor),]
ffsnoNA[is.na(ffsnoNA$Sand_Lance_Ind),7] <- 0
ffs1992 <- ffsnoNA[ffsnoNA$Year_ > 1990 &  ffsnoNA$Year_ < 1996,]
#ffs1992 <- ffs1992[ffs1992$Smelt_Ind ==1,]

boxplot(ffs1992$CosMos~  as.factor(ffs1992$Sand_Lance_Ind) + as.factor(ffs1992$ZoneFactor),col=c(3,2))
title("91-95 lance Indicator vs Region")
savePlot(paste(pldir,script,"\\MosByZone92.pdf",sep=""),type="pdf")

table(ffs1992$Month_)
tab92<- table(as.factor(ffs1992$ZoneFactor), as.factor(ffs1992$Sand_Lance_Ind))


tab92.prop <-tab92[,2]/(tab92[,1]+tab92[,2])
lanceInd92 <- recordPlot()

ffs2002 <- ffsnoNA[ffsnoNA$Year_ > 2002 &  ffsnoNA$Year_ < 2006,]
#ffs2002 <- ffs2002[ffs2002$Smelt_Ind ==1,]
boxplot(ffs2002$CosMos~ as.factor(ffs2002$Sand_Lance_Ind) + as.factor(ffs2002$ZoneFactor),col=c("Gray30","Orange"))
title("02-05 lance Indicator vs Region")
savePlot(paste(pldir,script,"\\MosByZone02.pdf",sep=""),type="pdf")
lanceInd02 <- recordPlot()
table(ffsall$Month_)
tab02 <- table(as.factor(ffs2002$ZoneFactor), as.factor(ffs2002$Sand_Lance_Ind))
tab02.prop <-tab02[,2]/(tab02[,1]+tab02[,2])


ss.det<-tapply(ffsnoNA$Sand_Lance_Ind,ffsnoNA$Year_,sum)
ss.surv<-table(ffsnoNA$Year)
ss.prop<- ss.det/ss.surv
barplot(ss.prop)
title("sand lance detection proportion 4 regions")
savePlot(paste(pldir,script,"\\YearlylanceDetectionRate.pdf",sep=""),type="pdf")
#barplot(rbind((ss.prop*ss.det),ss.surv))
barplot(ss.surv)
barplot(ss.prop*ss.surv, add=TRUE,col=2)
title("sand lance detection vs survey count 4 regions")
savePlot(paste(pldir,script,"\\YearlylanceDetAndSurvey.pdf",sep=""),type="pdf")
boxplot(ffsnoNA$CosMos~ as.factor(ffsnoNA$Sand_Lance_Ind) + as.factor(ffsnoNA$ZoneFactor),col=c("Gray30","Orange"))
title("lance Indicator vs Region")
savePlot(paste(pldir,script,"\\MosByZoneAllLance.pdf",sep=""),type="pdf")

