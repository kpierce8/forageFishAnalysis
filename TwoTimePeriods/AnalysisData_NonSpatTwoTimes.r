#Repeats much of AnalysisNonSpat repeated for plots in two different time periods

#Separate data into temporal categories
shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\AD_NonSpat2", sep=""))
script <- "\\AD_NonSpat2"





yrdata1972 <-ffsall[ffsall$Year_ %in% c(1972:1975),]
yrdata1982 <-ffsall[ffsall$Year_ %in% c(1980:1985),]
yrdata1992 <-ffsall[ffsall$Year_ %in% c(1992:1995),]
yrdata2002 <-ffsall[ffsall$Year_ %in% c(2001:2004),]

beach72<- unique(yrdata1972[,23])
beach82<- unique(yrdata1982[,23])
beach92<- unique(yrdata1992[,23])
beach02<- unique(yrdata2002[,23])

b92 <- beach92[beach92 %in% beach02]
b82 <- beach82[(beach82 %in% b92)]
b72 <- beach72[beach72 %in% b82]

yr0292 <- yrdata2002[yrdata2002[,23] %in% b92,]
yr9202 <- yrdata1992[yrdata1992[,23] %in% b92,]



#########################
## 1992 Dataset
#########################
ffsallYR <- yr9202

m1 <- match(szlinePF$UNIT_ID,szline$UNIT_ID)

szline<- szline[m1,]
bmatch <- match(ffsallYR$szline_Near_UNIT_ID,szline$UNIT_ID)
length(unique(ffsallYR$szline_Near_UNIT_ID))
ffssz <-  data.frame(ffsallYR, szline[bmatch,c(4,7,8,9)],1)

unit_counts <- table(ffssz$szline_Near_UNIT_ID)
unit_counts <- cbind(as.numeric(row.names(unit_counts)),as.numeric(unit_counts))
unit_counts <- data.frame(unit_counts)
names(unit_counts) <- c("OBJECT_ID","surveys")

unit_yrrange <- tapply(ffsallYR$Year_,ffsallYR$szline_Near_UNIT_ID,max)- tapply(ffsallYR$Year_,ffsallYR$szline_Near_UNIT_ID,min)
                                                                  
lancesum <- as.numeric(tapply(ffsallYR$Sand_Lance_Ind,ffsallYR$szline_Near_UNIT_ID,sum,na.rm=TRUE))
smeltsum <- as.numeric(tapply(ffsallYR$Smelt_Ind,ffsallYR$szline_Near_UNIT_ID,sum,na.rm=TRUE))
smeltbeach <- as.numeric(tapply(ffsallYR[ffsallYR$Smelt_Ind >0,23],ffsallYR[ffsallYR$Smelt_Ind >0,23],mean,na.rm=TRUE))
beachind <- unit_counts[,1] %in% smeltbeach

lanceprop <- lancesum/unit_counts$surveys
smeltprop <- smeltsum/unit_counts$surveys
ecoord <- as.numeric(tapply(ffsallYR$East_Coord,ffsallYR$szline_Near_UNIT_ID,mean,na.rm=TRUE))
ncoord <- as.numeric(tapply(ffsallYR$North_Coord,ffsallYR$szline_Near_UNIT_ID,mean,na.rm=TRUE))

unit_data <- data.frame(unit_counts, yrrange=as.numeric(unit_yrrange),lancesum,lanceprop,smeltsum,smeltprop,ecoord,ncoord,smeltmos=NA)
unit_data$smeltmos[beachind] <- smeltmos

dim(unit_data)
summary(unit_data)

smatch<-match(szline$OBJECTID, unit_counts[,1])

szcounts92<-  data.frame(szline,unit_data[smatch,])
write.dbf(unit_data,file="c:/unit_data2time.dbf")

dim(szcounts92)
#summary(szcounts92)

#####PLOTS 1992
# Beach surveys by frequency
hist(unit_counts[,2],breaks="fd",xlim=c(0,100),xlab="Beach Surveys",main="SZ Beaches by Visit Frequency")
text(75,250,"7 beaches w>100 visits, 1567 surveys")
text(75,200,"3689 of 6956 beaches surveyed")
savePlot(paste(pldir,script,"\\BeachSurveyFreq92.pdf",sep=""),type="pdf")
# Beach length by counts (log)
plot(log(szcounts92$LENGTH),log(szcounts92$surveys)+rnorm(dim(szcounts92)[1])/30,xlab="log(Beach length (ft))", ylab= "log(FFS surveys)",cex=.25)
savePlot(paste(pldir,script,"\\LogBeachLength92.pdf",sep=""),type="pdf")


# Beach length by counts (truncated)
plot(szcounts92$LENGTH, szcounts92$surveys+rnorm(dim(szcounts92)[1])/5, xlab="Beach length (ft)", ylab="FFS surveys (1/5 SD jittered)", cex=.25, xlim=c(0,10000),ylim=c(0,100))
savePlot(paste(pldir,script,"\\BeachLength92.pdf",sep=""),type="pdf")

# Hist of year ranges
hist(unit_yrrange)
savePlot(paste(pldir,script,"\\HistYrRange92.pdf",sep=""),type="pdf")
# Survey quantity vs year range
plot(unit_data$surveys+rnorm(dim(unit_data)[1]), unit_data$yrrange+rnorm(dim(unit_data)[1]), xlim=c(1,40),cex=.25)
savePlot(paste(pldir,script,"\\SurveyvsYrRange92.pdf",sep=""),type="pdf")


#szcounts92[szcounts92$ZoneFactor == 3, 101]


boxplot(szcounts92$smeltmos ~ szcounts92$ZoneFactor)
savePlot(paste(pldir,script,"\\SmeltMosbyZone92.pdf",sep=""),type="pdf")

barplot(tapply(ffsall[,6],ffsall$Year_,sum,na.rm=TRUE),ylab="Smelt Observations")
savePlot(paste(pldir,script,"\\SmeltObservations92.pdf",sep=""),type="pdf")
barplot(tapply(ffsall[,7],ffsall$Year_,sum,na.rm=TRUE),ylab="Sand Lance Observations")
savePlot(paste(pldir,script,"\\LanceObservations92.pdf",sep=""),type="pdf")
barplot(table(ffsall$Year_),ylab="Surveys")
savePlot(paste(pldir,script,"\\Surveys92.pdf",sep=""),type="pdf")



unit_data92 <- unit_data
#######################################################################################
## 2002 Dataset
#######################################################################################
ffsallYR <- yr0292

m1 <- match(szlinePF$UNIT_ID,szline$UNIT_ID)

szline<- szline[m1,]
bmatch <- match(ffsallYR$szline_Near_UNIT_ID,szline$UNIT_ID)
length(unique(ffsallYR$szline_Near_UNIT_ID))
ffssz <-  data.frame(ffsallYR, szline[bmatch,c(4,7,8,9)],1)

unit_counts <- table(ffssz$szline_Near_UNIT_ID)
unit_counts <- cbind(as.numeric(row.names(unit_counts)),as.numeric(unit_counts))
unit_counts <- data.frame(unit_counts)
names(unit_counts) <- c("OBJECT_ID","surveys")

unit_yrrange <- tapply(ffsallYR$Year_,ffsallYR$szline_Near_UNIT_ID,max)- tapply(ffsallYR$Year_,ffsallYR$szline_Near_UNIT_ID,min)

lancesum <- as.numeric(tapply(ffsallYR$Sand_Lance_Ind,ffsallYR$szline_Near_UNIT_ID,sum,na.rm=TRUE))
smeltsum <- as.numeric(tapply(ffsallYR$Smelt_Ind,ffsallYR$szline_Near_UNIT_ID,sum,na.rm=TRUE))
smeltbeach <- as.numeric(tapply(ffsallYR[ffsallYR$Smelt_Ind >0,23],ffsallYR[ffsallYR$Smelt_Ind >0,23],mean,na.rm=TRUE))
smeltmos <- as.numeric(tapply(ffsallYR[ffsallYR$Smelt_Ind >0,16],ffsallYR[ffsallYR$Smelt_Ind >0,23],mean,na.rm=TRUE))
beachind <- unit_counts[,1] %in% smeltbeach

lanceprop <- lancesum/unit_counts$surveys
smeltprop <- smeltsum/unit_counts$surveys
ecoord <- as.numeric(tapply(ffsallYR$East_Coord,ffsallYR$szline_Near_UNIT_ID,mean,na.rm=TRUE))
ncoord <- as.numeric(tapply(ffsallYR$North_Coord,ffsallYR$szline_Near_UNIT_ID,mean,na.rm=TRUE))

unit_data <- data.frame(unit_counts, yrrange=as.numeric(unit_yrrange),lancesum,lanceprop,smeltsum,smeltprop,ecoord,ncoord,smeltmos=NA)
unit_data$smeltmos[beachind] <- smeltmos
dim(unit_data)
#summary(unit_data)

smatch<-match(szline$OBJECTID, unit_counts[,1])

szcounts02<-  data.frame(szline,unit_data[smatch,])
write.dbf(unit_data,file="c:/unit_dataBEACHES.dbf")

dim(szcounts02)
#summary(szcounts02)

#####PLOTS    2002
# Beach surveys by frequency
hist(unit_counts[,2],breaks="fd",xlim=c(0,100),xlab="Beach Surveys",main="SZ Beaches by Visit Frequency")
text(75,250,"7 beaches w>100 visits, 1567 surveys")
text(75,200,"3689 of 6956 beaches surveyed")
savePlot(paste(pldir,script,"\\BeachSurveyFreq02.pdf",sep=""),type="pdf")
# Beach length by counts (log)
plot(log(szcounts02$LENGTH),log(szcounts02$surveys)+rnorm(dim(szcounts02)[1])/30,xlab="log(Beach length (ft))", ylab= "log(FFS surveys)",cex=.25)
savePlot(paste(pldir,script,"\\LogBeachLength02.pdf",sep=""),type="pdf")


# Beach length by counts (truncated)
plot(szcounts02$LENGTH, szcounts02$surveys+rnorm(dim(szcounts02)[1])/5, xlab="Beach length (ft)", ylab="FFS surveys (1/5 SD jittered)", cex=.25, xlim=c(0,10000),ylim=c(0,100))
savePlot(paste(pldir,script,"\\BeachLength02.pdf",sep=""),type="pdf")
# Hist of year ranges
hist(unit_yrrange)
savePlot(paste(pldir,script,"\\HistYrRange02.pdf",sep=""),type="pdf")
# Survey quantity vs year range
plot(unit_data$surveys+rnorm(dim(unit_data)[1]), unit_data$yrrange+rnorm(dim(unit_data)[1]), xlim=c(1,40),cex=.25)
savePlot(paste(pldir,script,"\\SurveyvsYrRange02.pdf",sep=""),type="pdf")


barplot(tapply(ffsall[,6],ffsall$Year_,sum,na.rm=TRUE),ylab="Smelt Observations")
savePlot(paste(pldir,script,"\\SmeltObservations02.pdf",sep=""),type="pdf")
barplot(tapply(ffsall[,7],ffsall$Year_,sum,na.rm=TRUE),ylab="Sand Lance Observations")
savePlot(paste(pldir,script,"\\LanceObservations02.pdf",sep=""),type="pdf")
barplot(table(ffsall$Year_),ylab="Surveys")
savePlot(paste(pldir,script,"\\Surveys02.pdf",sep=""),type="pdf")





unit_data02 <- unit_data

dim(unit_data)

plot(unit_data02$smeltprop + (rnorm(942)/100) ,unit_data92$smeltprop + (rnorm(942)/100),pch=20)


t.test(unit_data02$smeltprop,unit_data92$smeltprop)


windows(width=8, height=11,xpinch=72,ypinch=72)
#par(mfrow=c(2,3),mai=c(.25,.1,.1,.25))


plot(ffsall[,11],ffsall[,12],pch=".",xlim=c(700000,1250000),ylim=c(600000,1400000))
yrdata<-ffsallYR

par(pty="s")
points(yrdata$East_Coord,yrdata$North_Coord,col=2,pch=16)
#points(yrsmelt$East_Coord,yrsmelt$North_Coord,col=3,pch=20)
text(1220000,1350000,years[i])

time2_data <- rbind(unit_data92,unit_data02)
write.dbf(time2_data,file="c:/unit_data2time.dbf")



ffsnoNA <- ffsall[!is.na(ffsall$ZoneFactor),]
ffsnoNA[is.na(ffsnoNA$Smelt_Ind),6] <- 0
ffs1992 <- ffsnoNA[ffsnoNA$Year_ > 1990 &  ffsnoNA$Year_ < 1996,]
#ffs1992 <- ffs1992[ffs1992$Smelt_Ind ==1,]

boxplot(ffs1992$CosMos~  as.factor(ffs1992$Smelt_Ind) + as.factor(ffs1992$ZoneFactor))
title("91-95 Smelt Indicator vs Region")
table(ffs1992$Month_)
table(as.factor(ffs1992$ZoneFactor), as.factor(ffs1992$Smelt_Ind))
smeltInd92 <- recordPlot()


ffs2002 <- ffsnoNA[ffsnoNA$Year_ > 2002 &  ffsnoNA$Year_ < 2006,]
#ffs2002 <- ffs2002[ffs2002$Smelt_Ind ==1,]
boxplot(ffs2002$CosMos~ as.factor(ffs2002$Smelt_Ind) + as.factor(ffs2002$ZoneFactor))
title("02-05 Smelt Indicator vs Region")
smeltInd02 <- recordPlot()
table(ffsall$Month_)
table(as.factor(ffs2002$ZoneFactor), as.factor(ffs2002$Smelt_Ind))


