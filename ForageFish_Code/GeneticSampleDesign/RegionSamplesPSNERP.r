shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\RegionSamplesPSNERP", sep=""))
script <- "\\RegionSamplesPSNERP"
cscheme <- 0
library(RColorBrewer)
par(par_reset)
points<- read.dbf("L:\\gis_data_mgmt\\sshiap\\working_pierce\\ForageFish\\SurveysOnEither_Identity.dbf")
hexs <- read.dbf("L:\\gis_data_mgmt\\sshiap\\working_pierce\\ForageFish\\hexagons.dbf")

rgbm <- function(x,y,z){rgb(x,y,z,maxColorValue = 255)}
reramp <- c(rgbm(228,26,28),rgbm(55,126,184),rgbm(77,175,74),rgbm(152,78,163),rgbm(255,127,0),rgbm(0,0,0),rgbm(255,255,51),rgbm(166,86,40))

#
#table(hexs$SampleRegi)
#regions<-unique(hexs$SampleRegi)
#regions<-regions[!is.na(regions)]
#samp_hexs1 <- c()
#samp_hexs2 <- c()
#for(i in 1:length(regions)){
#reg <- regions[i]
#subhex <- subset(hexs,hexs$SampleRegi == reg)
#
#sample1<-sample(length(subhex[,1]),30)
#sample2<-sample(length(subhex[,1]),30)
#samp_hexs1 <- c(samp_hexs1,subhex[sample1,1])
#samp_hexs2 <- c(samp_hexs2,subhex[sample2,1])
#
#}
#plot(samp_hexs1,samp_hexs2)
#hist(table(points$FID_hexago),breaks=c(seq(1:50),300),xlim=c(0,50),freq=T)
#savePlot(paste(pldir,script,"\\PointsperHexHist.wmf",sep=""),type="wmf")
#########################################


#samp_survs1 <- c()
#samp_survs2 <- c()
#for(i in 1:length(samp_hexs1)){
#reg1 <- samp_hexs1[i]
#subhex1 <- subset(points,points$Unique_ID == reg1)
#sample1<-sample(length(subhex1[,3]),1)
#samp_survs1 <- c(samp_survs1,subhex1[sample1,3])
#
#reg2 <- samp_hexs2[i]
#subhex2 <- subset(points,points$Unique_ID == reg2)
#sample2<-sample(length(subhex2[,3]),1)
#samp_survs2 <- c(samp_survs2,subhex2[sample2,3])
#
#}
#length(samp_survs1)
#length(samp_survs2)
#
#plot(samp_survs1,samp_survs2)
#savePlot(paste(pldir,script,"\\Samps.wmf",sep=""),type="wmf")
#
#
#write.dbf(samp_survs1,"e:/foragefish/samp1.dbf")
#write.dbf(samp_survs2,"e:/foragefish/samp2.dbf")
#
################################################################
par(par_reset)
regions<-unique(ffsall$PSNERP_SUB)
#regions<-regions[!is.na(regions)]
#samp_hexs1 <- c()
#samp_hexs2 <- c()
#for(i in 1:length(regions)){
#reg <- regions[i]
#subhex <- subset(points,points$SampleRegi == reg)
#boxplot(subhex$CosMos)
#boxplot(subhex[subhex[,6] == 1,16],add=T)
#}
#
##PSNERP changes below
boxplot(points$CosMos~ as.factor(points$Smelt_Ind))
sm_surveys <- subset(ffsall,ffsall$Smelt_Ind ==1)
la_surveys <- subset(ffsall,ffsall$Sand_Lance_Ind ==1) 
colram<-sort(unique(sm_surveys$PSNERP_SUB))
par(ps=20)
boxplot(ffsall$CosMos ~ as.factor(ffsall$PSNERP_SUB),col=reramp[colram],main="Surveys by Region", xlab=as.factor(ffsall$PSNERP_SUB))
#axis(1,at=c(1:7),seq(1,7))
savePlot(paste(pldir,script,"\\SurveysByRegion.wmf",sep=""),type="wmf")






boxplot(sm_surveys$CosMos ~ as.factor(sm_surveys$PSNERP_SUB),col=reramp[colram],main="Surf Smelt by Region", xaxt="n")
axis(1,at=c(1:7),seq(1,7))
savePlot(paste(pldir,script,"\\SmeltByRegion.wmf",sep=""),type="wmf")
boxplot(la_surveys$CosMos ~ as.factor(la_surveys$PSNERP_SUB),col=reramp[colram],main="Sand Lance by Region",ylim=c(-1,1),xaxt="n")
axis(1,at=c(1:7),seq(1,7))
savePlot(paste(pldir,script,"\\LanceByRegion.wmf",sep=""),type="wmf")



both_surveys <- subset(la_surveys,la_surveys$Smelt_Ind ==1)
dim(both_surveys)
#both_surveys$Month_
table(both_surveys$Month_)
table(la_surveys$Month_)
table(sm_surveys$Month_)
table(ffsall$Month_)
table(sm_surveys$Month_)/table(ffsall$Month_)
barplot(table(sm_surveys$Month_)/table(ffsall$Month_))
table(ffsall$Month_)/sum(table(ffsall$Month_))


boxplot(ffsall$CosMos ~ ffsall$NorthSouth)

south_surv <- subset(ffsall,ffsall$NorthSouth == 1)
north_surv <- subset(ffsall,ffsall$NorthSouth == 2)

table(south_surv$Month_)
table(north_surv$Month_)
par(ps=16)
par(mfrow=c(2,1),mai=c(.5,.5,.5,.5))
barplot(table(north_surv$Month_),col=4,ylab="N Surveys")
#abline(h=500)
barplot(table(south_surv$Month_),col="gray50",ylab="S Surveys", xlab="Month")
#abline(h=500)
savePlot(paste(pldir,script,"\\NorSouSurvs.wmf",sep=""),type="wmf")

south_sm <- subset(sm_surveys,sm_surveys$NorthSouth == 1)
north_sm <- subset(sm_surveys,sm_surveys$NorthSouth == 2)


barplot(table(north_sm$Month_)/table(north_surv$Month_),col="brown",ylab="N Smelt prop",ylim=c(0,.5),cex.lab=2)
#abline(h=.2)
barplot(table(south_sm$Month_)/table(south_surv$Month_),col="yellow",ylab="S Smelt prop",ylim=c(0,.5), xlab="Month")
#abline(h=.2)
savePlot(paste(pldir,script,"\\NorSouSmeltProp.wmf",sep=""),type="wmf")

###################
south_la <- subset(la_surveys,la_surveys$NorthSouth == 1)
north_la <- subset(la_surveys,la_surveys$NorthSouth == 2)

t1<-table(north_la$Month_)
t12 <- rep(0,12)
t12[as.numeric(row.names(t1))]<- t1
t2<-table(north_surv$Month_)
#gt<- match(names(t1),names(t2))
barplot(t12/t2,col="brown",ylab="N Lance prop",ylim=c(0,.5))
#abline(h=.2)
t1<-table(south_la$Month_)
t12 <- rep(0,12)
t12[as.numeric(row.names(t1))]<- t1
t2<-table(south_surv$Month_)
#gt<- match(names(t1),names(t2))
barplot(t12/t2,col="yellow",ylab="S Lance prop", xlab="Month",ylim=c(0,.5))
#abline(h=.2)
savePlot(paste(pldir,script,"\\NorSouLanceProp.wmf",sep=""),type="wmf")
######################

tapply(north_sm[,26],north_sm$Month_,mean,na.rm=T)
tapply(south_sm[,26],south_sm$Month_,mean,na.rm=T)

jan_surv <- subset(ffsall,ffsall$Month_ == 1)
jan_surv[is.na(jan_surv$Smelt_Ind),6] <- 0
jan_sm <- glm(jan_surv$Smelt_Ind ~ jan_surv$North_Coord, family = binomial(link= "logit"))

jun_surv <- subset(ffsall,ffsall$Month_ == 6)
jun_surv[is.na(jun_surv$Smelt_Ind),6] <- 0
jun_sm <- glm(jun_surv$Smelt_Ind ~ jun_surv$North_Coord, family = binomial(link= "logit"))
display(jun_sm)

jun_surv <- subset(ffsall,ffsall$Month_ == 12)
jun_surv[is.na(jun_surv$Smelt_Ind),6] <- 0
dec_sm <- glm(jun_surv$Smelt_Ind ~ jun_surv$North_Coord, family = binomial(link= "logit"))
display(dec_sm)
