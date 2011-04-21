##This analysis is based on the four arbitrary regions and their perceived 
#temporal differences between the 2 time periods
shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\Reg2times", sep=""))
script <- "\\Reg2times"

library(RODBC)
ddir <- c("e")

FFS <- odbcConnectAccess(paste(ddir,":/ForageFish/Pierce/myForageFish.mdb",sep=""))

ffsall <- sqlFetch(FFS, "FFS_all")
szline <- sqlFetch(FFS, "szline")


ffsnoNA <- ffsall[!is.na(ffsall$ZoneFactor),]
ffsnoNA[is.na(ffsnoNA$Smelt_Ind),6] <- 0
ffs1992 <- ffsnoNA[ffsnoNA$Year_ > 1990 &  ffsnoNA$Year_ < 1996,]
#ffs1992 <- ffs1992[ffs1992$Smelt_Ind ==1,]

boxplot(ffs1992$CosMos~  as.factor(ffs1992$Smelt_Ind) + as.factor(ffs1992$ZoneFactor),col=c(3,2))
title("91-95 Smelt Indicator vs Region")
savePlot(paste(pldir,script,"\\MosByZone92.pdf",sep=""),type="pdf")

table(ffs1992$Month_)
tab92<- table(as.factor(ffs1992$ZoneFactor), as.factor(ffs1992$Smelt_Ind))


tab92.prop <-tab92[,2]/(tab92[,1]+tab92[,2])
smeltInd92 <- recordPlot()

ffs2002 <- ffsnoNA[ffsnoNA$Year_ > 2002 &  ffsnoNA$Year_ < 2006,]
#ffs2002 <- ffs2002[ffs2002$Smelt_Ind ==1,]
boxplot(ffs2002$CosMos~ as.factor(ffs2002$Smelt_Ind) + as.factor(ffs2002$ZoneFactor),col=c("Gray30","Orange"))
title("02-05 Smelt Indicator vs Region")
savePlot(paste(pldir,script,"\\MosByZone02.pdf",sep=""),type="pdf")
smeltInd02 <- recordPlot()
table(ffsall$Month_)
tab02 <- table(as.factor(ffs2002$ZoneFactor), as.factor(ffs2002$Smelt_Ind))
tab02.prop <-tab02[,2]/(tab02[,1]+tab02[,2])


ss.det<-tapply(ffsnoNA$Smelt_Ind,ffsnoNA$Year_,sum)
ss.surv<-table(ffsnoNA$Year)
ss.prop<- ss.det/ss.surv
barplot(ss.prop)
title("Surf Smelt detection proportion 4 regions")
savePlot(paste(pldir,script,"\\YearlySmeltDetectionRate.pdf",sep=""),type="pdf")
#barplot(rbind((ss.prop*ss.det),ss.surv))
barplot(ss.surv)
barplot(ss.prop*ss.surv, add=TRUE,col=2)
title("Surf Smelt detection vs survey count 4 regions")
savePlot(paste(pldir,script,"\\YearlySmeltDetAndSurvey.pdf",sep=""),type="pdf")
boxplot(ffsnoNA$CosMos~ as.factor(ffsnoNA$Smelt_Ind) + as.factor(ffsnoNA$ZoneFactor),col=c("Gray30","Orange"))
title("Smelt Indicator vs Region")
savePlot(paste(pldir,script,"\\MosByZoneAll.pdf",sep=""),type="pdf")