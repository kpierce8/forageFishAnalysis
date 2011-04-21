library(RODBC)

ddir <- c("e")

shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\AD_NonSpat", sep=""))
script <- "\\AD_NonSpat"



FFS <- odbcConnectAccess(paste(ddir,":/ForageFish/myForageFish.mdb",sep=""))
ffsall <- sqlFetch(FFS, "FFS_all")  #Survey data
szline <- sqlFetch(FFS, "szline")   #Beach data
szlinePF <- sqlFetch(FFS, "szlinePF")
szlinePFsing <- sqlFetch(FFS, "szlinePF_single")
ffsall$moscut <- cut(ffsall$CosMos,5,labels=c(1,2,3,4,5))                                    

m1 <- match(szlinePF$UNIT_ID,szline$UNIT_ID)
ffsall[is.na(ffsall$Smelt_Ind),6] <-0
ffsall[is.na(ffsall$Sand_Lance_Ind),7] <-0
szline<- szline[m1,]
bmatch <- match(ffsall$szline_Near_UNIT_ID,szline$UNIT_ID)
length(unique(ffsall$szline_Near_UNIT_ID))
ffssz <-  data.frame(ffsall, szline[bmatch,c(4,7,8,9)],1)

###############################################################################
##CALC per beach statistics
# surveys per beach
# proportion finding eggs
# range of years of visits
###############################################################################
unit_counts <- table(ffssz$szline_Near_UNIT_ID)
unit_counts <- cbind(as.numeric(row.names(unit_counts)),as.numeric(unit_counts))
unit_counts <- data.frame(unit_counts)
names(unit_counts) <- c("OBJECT_ID","surveys")

#Create year range from max and min year-beach combo
unit_yrrange <- tapply(ffsall$Year_,ffsall$szline_Near_UNIT_ID,max)- tapply(ffsall$Year_,ffsall$szline_Near_UNIT_ID,min)

lancesum <- as.numeric(tapply(ffsall$Sand_Lance_Ind,ffsall$szline_Near_UNIT_ID,sum,na.rm=TRUE))
smeltsum <- as.numeric(tapply(ffsall$Smelt_Ind,ffsall$szline_Near_UNIT_ID,sum,na.rm=TRUE))

lanceprop <- lancesum/unit_counts$surveys
smeltprop <- smeltsum/unit_counts$surveys
ecoord <- as.numeric(tapply(ffsall$East_Coord,ffsall$szline_Near_UNIT_ID,mean,na.rm=TRUE))
ncoord <- as.numeric(tapply(ffsall$North_Coord,ffsall$szline_Near_UNIT_ID,mean,na.rm=TRUE))


#years visited analysis, trying to look at probability of visit after egg observation
yrmed <- as.numeric(tapply(ffsall$Year_,ffsall$szline_Near_UNIT_ID,median,na.rm=TRUE))
yrmax <- as.numeric(tapply(ffsall$Year_,ffsall$szline_Near_UNIT_ID,max,na.rm=TRUE))
yrmin <- as.numeric(tapply(ffsall$Year_,ffsall$szline_Near_UNIT_ID,min,na.rm=TRUE))
ffsFISH<- ffsall[ffsall$Smelt_Ind == 1,]
yrmedF <- as.numeric(tapply(ffsFISH$Year_,ffsFISH$szline_Near_UNIT_ID,median,na.rm=TRUE))
yrmaxF <- as.numeric(tapply(ffsFISH$Year_,ffsFISH$szline_Near_UNIT_ID,max,na.rm=TRUE))
bset <- unique(ffsFISH[,23])
ffsmat <- ffsall[ffsall[,23] %in% bset,]
yrmaxFM <- as.numeric(tapply(ffsmat$Year_,ffsmat$szline_Near_UNIT_ID,max,na.rm=TRUE))
hist(yrmaxFM-yrmaxF)
boxplot(yrmaxFM-yrmedF,yrmax-yrmin)
lastvisit<- yrmaxFM[yrmaxFM-yrmaxF == 0]
hist(lastvisit)

###
#Create SZLINE data table
unit_data <- data.frame(unit_counts, yrrange=as.numeric(unit_yrrange),lancesum,lanceprop,smeltsum,smeltprop,ecoord,ncoord)


mPSNERP<-match(unit_data$OBJECT_ID, szline$UNIT_ID)
unit_data <- data.frame(unit_data, PSNERP=szline$PSNERPsub[mPSNERP])

dim(unit_data)
summary(unit_data)



##End data import


smatch<-match(szline$OBJECTID, unit_counts[,1])

szcounts<-  data.frame(szline,unit_data[smatch,])
write.dbf(unit_data,file="f:/foragefish/unit_data.dbf")

dim(szcounts)
summary(szcounts)

#####PLOTS
# Beach surveys by frequency
hist(unit_counts[,2],breaks="fd",xlim=c(0,100),xlab="Beach Surveys",main="ShoreZone Beaches by Visit Frequency")
text(75,250,"7 beaches w>100 visits, 1567 surveys")
text(75,200,"3689 of 6956 beaches surveyed")
savePlot(paste(pldir,script,"\\BeachSurveyFreq.pdf",sep=""),type="pdf")

# Beach surveys by frequency   (POSTER PLOT)
hist(unit_counts[,2],breaks=c(seq(0,40),500),freq=TRUE,xlim=c(0,40),col=2,xlab="Beach Surveys",main="")
text(23,800,"Total surveys 20,294",cex=2)
text(23,600,"26 beaches with >40 surveys",cex=2)
text(23,700,"3689 of 6956 beaches surveyed",cex=2)
savePlot(paste(pldir,script,"\\BeachSurveyFreqPoster.wmf",sep=""),type="w")





# Beach length by counts (log)
plot(log(szcounts$LENGTH),log(szcounts$surveys)+rnorm(dim(szcounts)[1])/30,xlab="log(Beach length (ft))", ylab= "log(FFS surveys)",cex=.25)
# Beach length by counts (truncated)
savePlot(paste(pldir,script,"\\LogBeachLength.pdf",sep=""),type="pdf")

plot(szcounts$LENGTH, szcounts$surveys+rnorm(dim(szcounts)[1])/5, xlab="Beach length (ft)", ylab="FFS surveys (1/5 SD jittered)", cex=.25, xlim=c(0,10000),ylim=c(0,100))
savePlot(paste(pldir,script,"\\BeachLength.pdf",sep=""),type="pdf")
# Hist of year ranges
hist(unit_yrrange)
savePlot(paste(pldir,script,"\\HistYrRange.pdf",sep=""),type="pdf")

# Survey quantity vs year range
plot(unit_data$surveys+rnorm(dim(unit_data)[1]), unit_data$yrrange+rnorm(dim(unit_data)[1]), xlim=c(1,40),cex=.25)
savePlot(paste(pldir,script,"\\SurveyvsYrRange.pdf",sep=""),type="pdf")

barplot(tapply(ffsall[,6],ffsall$Year_,sum,na.rm=TRUE),ylab="Smelt Observations")
savePlot(paste(pldir,script,"\\SmeltObservations.pdf",sep=""),type="pdf")
barplot(tapply(ffsall[,7],ffsall$Year_,sum,na.rm=TRUE),ylab="Sand Lance Observations")
savePlot(paste(pldir,script,"\\LanceObservations.pdf",sep=""),type="pdf")
barplot(table(ffsall$Year_),ylab="Surveys")
savePlot(paste(pldir,script,"\\Surveys.pdf",sep=""),type="pdf")



ffsall$colcut<- cut(ffsall$SmeltEggsPercLive,5,labels=c(1,2,3,4,5))
#ffsall<-data.frame(ffsall,colcut=temp1)

#Surveys by year
windows(width=11, height=8,xpinch=72,ypinch=72)
par(mfrow=c(2,3),mai=c(.25,.1,.1,.25))
years<- sort(unique(ffsall$Year_))
for(i in 1:34){
plot(ffsall[,11],ffsall[,12],pch=".",xlim=c(900000,1250000),ylim=c(600000,1400000))
yrdata<-ffsall[ffsall$Year_ == years[i],]
yrsmelt <- yrdata[yrdata[,6] == 1,]
dim(yrdata)
par(pty="s")
points(yrdata$East_Coord,yrdata$North_Coord,col=2,pch=16)
points(yrsmelt$East_Coord,yrsmelt$North_Coord,col=3,pch=20)
text(1220000,1350000,years[i])
if(i %% 6 == 0) savePlot(filename=paste(pldir,script,"\\surveys",i,".pdf",sep=""),type="pdf")
}

savePlot(filename=paste(pldir,script,"/surveys",i,".pdf",sep=""),type="pdf")

par(mfrow=c(1,1))
plot(ffsall[,11],ffsall[,12],pch=".",xlim=c(900000,1250000),ylim=c(600000,1400000))

library(sfsmisc)
library(plotrix)
cbob<- color.gradient(c(1,0,0),c(0,1,0),c(0,0,1),nslices=dim(unit_data)[1])
unit_smelt <- unit_data[order(unit_data$smeltprop),]
points(unit_smelt$ecoord,unit_smelt$ncoord,col=cbob,pch=20)

savePlot(filename=paste(pldir,script,"/surveysALL.pdf",sep=""),type="pdf")

write.dbf(unit_data,file="c:/data/unit_data.dbf")

##Surveys by year colored by Winter/Summerness
#windows(width=11, height=8,xpinch=72,ypinch=72)
#par(mfrow=c(2,3),mai=c(.25,.1,.1,.25))
par(par_reset)
colramp<- heat.colors(5)
cbob<- color.gradient(c(1,0,0),c(0,1,0),c(0,0,1),nslices=5)
years<- sort(unique(ffsall$Year_))
for(i in 1:34){
plot(ffsall[,11],ffsall[,12],pch=".",xlim=c(500000,1300000),ylim=c(600000,1400000),xlab="easting",ylab="northing")
yrdata<-ffsall[ffsall$Year_ == years[i],]
yrsmelt <- yrdata[yrdata[,6] == 1,]

dim(yrdata)
par(pty="s")

points(yrdata$East_Coord,yrdata$North_Coord,col=cbob[yrdata$moscut],pch=16)
#points(yrsmelt$East_Coord,yrsmelt$North_Coord,col=3,pch=20)
text(1220000,1350000,years[i])
title("Locations of Smelt Egg Observations \n color-coded by survey season",sub="")

legend(650000,1380000,legend=c("Summer","Summerish","Equinox","Winterish","Winter"),pch=20,col=cbob)
if(i %% 1 == 0) savePlot(filename=paste(pldir,script,"\\SeasonByYear",i,".jpg",sep=""),type="jpg")
}

######## PSNERP Regional analyses
## by sample location
smelt <- ffsall[ffsall$Smelt_Ind ==1,]

regionall<-as.matrix(table(ffsall$PSNERP_SUB,ffsall$Month_))
regionmos<-as.matrix(table(smelt$PSNERP_SUB,smelt$Month_))
fifthcentile<- .05 * apply(regionmos,1,sum)
regperc <- sig3(regionmos/regionall,dig=2)

mos <- 7
sort(regionmos[mos,])
cumsum(sort(regionmos[mos,]))
fifthcentile[mos]


## by shore zone beach
gt2visits <- szcounts[szcounts$surveys > 1,]
table(gt2visits$smeltsum)

ni(ffsall)

plot(jitter(szcounts$surveys,factor=2),jitter(szcounts$smeltprop,factor=100),pch=".",xlim=c(0,20))

smelt_persistence<-table(szcounts[,c(104,108)])
smelt_persistence[,1:10]

lance_persistence<-table(szcounts[,c(104,106)])
lance_persistence[,1:10]


allcount <-  szcounts[,106]+szcounts[,108]
sum_persistence<-table(szcounts[,104],allcount)
sum_persistence[1:40,1:10]

all_persist_percent<-sig3(sum_persistence/apply(sum_persistence,1,sum))
all_persist_percent[,1:10]

perc_10visits<-apply(all_persist_percent[,1],1,sum)
sum_counts<-apply(sum_persistence,1,sum)

cbind(sum_counts,perc_10visits)

bob<- match(c("40"),row.names(all_persist_percent))   ;bob

all_persist_percent[bob,]
