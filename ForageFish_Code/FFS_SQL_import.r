shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\FFS_SQL", sep=""))
script <- "\\FFS_SQL"



FFS <- odbcConnectAccess(paste(ddir,":/ForageFish/FF_SQL_Copy.mdb",sep=""))

q1<- "TRANSFORM Count(EGG_STAGE_LUT.Egg_Stage_Desc) AS CountOfEgg_Stage_Desc"
q2<- "SELECT SURVEY_D.Survey_Id FROM EGG_STAGE_LUT INNER JOIN (((BEACH_D INNER JOIN (SURVEY_D INNER JOIN (SUBSTRATE_SAMPLE_D INNER JOIN EGG_COUNT_D ON (SUBSTRATE_SAMPLE_D.Substrate_Seq_Num = EGG_COUNT_D.Substrate_Seq_Num) AND (SUBSTRATE_SAMPLE_D.Survey_Id = EGG_COUNT_D.Survey_Id)) ON SURVEY_D.Survey_Id = SUBSTRATE_SAMPLE_D.Survey_Id) ON BEACH_D.Beach_Id = SURVEY_D.Beach_Id) INNER JOIN SPECIES_LUT ON EGG_COUNT_D.Species_LUT_Id = SPECIES_LUT.Species_LUT_Id) INNER JOIN EGG_STAGE_D ON EGG_COUNT_D.Egg_Count_Id = EGG_STAGE_D.Egg_Count_Id) ON EGG_STAGE_LUT.Egg_Stage_LUT_Id = EGG_STAGE_D.Egg_Stage_LUT_Id"
q3<- "WHERE (((EGG_COUNT_D.Species_LUT_Id)=1)) GROUP BY SURVEY_D.Survey_Id PIVOT EGG_STAGE_LUT.Egg_Stage_Desc;"


table1<-sqlQuery(FFS,paste(q1,q2,q3))

column_order <- c(1,4,7,9,3,5,6,2,10,8)

table1 <- table1[,column_order]
table1[is.na(table1)] <- 0
ni(table1)

table1<-transform(table1, live = apply(table1[,2:9],1,sum),eggs = apply(table1[,2:10],1,sum))
table1<-transform(table1, proplive = live / eggs)

q4<- "WHERE (((EGG_COUNT_D.Species_LUT_Id)=2)) GROUP BY SURVEY_D.Survey_Id PIVOT EGG_STAGE_LUT.Egg_Stage_Desc;"
table2<-sqlQuery(FFS,paste(q1,q2,q4))
column_order <- c(1,4,7,9,3,5,6,2,10,8)
table2 <- table2[,column_order]
ni(table2)
table2[is.na(table2)] <- 0
table2<-transform(table2, live = apply(table2[,2:9],1,sum),eggs = apply(table2[,2:10],1,sum))
table2<-transform(table2, proplive = live / eggs)

write.dbf(table1,paste(ddir,":/foragefish/smelt_eggs.dbf",sep=""))
write.dbf(table2,paste(ddir,":/foragefish/lance_eggs.dbf",sep=""))



smatch <- ffsall$Survey_Id %in% table1$Survey_Id
ffsall_eggs_smelt <- ffsall[smatch,]
smatch <- match(ffsall_eggs_smelt$Survey_Id,table1$Survey_Id)
ffsall_eggs_smelt <- data.frame(ffsall_eggs_smelt,table1[smatch,])
fes <- ffsall_eggs_smelt
cor(fes[,3],fes[,26])

par(par_reset)
plot(ffsall[,11],ffsall[,12],pch=".",xlim=c(900000,1250000),ylim=c(600000,1400000))

library(sfsmisc)
library(plotrix)
cbob<- color.gradient(c(1,0,0),c(0,1,0),c(0,0,1),nslices=dim(fes)[1])
prop_smelt <- fes[order(fes$proplive),]
points(fes[,11],fes[,12],col=cbob,pch=20, cex=.65)
title("Prop-live Smelt")
names(fes)<-gsub('\\.', '', names(fes),extended=T)

write.table(fes,paste(ddir,":/foragefish/fes2.csv",sep=""),sep=",")
par(mfrow=c(1,1))
ssliveMos<-tapply(ffsall$SmeltEggsPercLive,ffsall$Month_,mean,na.rm=T)
barplot(ssliveMos)
savePlot(paste(pldir,script,"\\ssliveMos.pdf",sep=""),type="pdf")


ssliveType<-tapply(ffsall$SmeltEggsPercLive,ffsall$BeachType,mean,na.rm=T)
barplot(ssliveType)
savePlot(paste(pldir,script,"\\ssliveType.pdf",sep=""),type="pdf")

# Sand Lance Eggpercentages
getSLprop <- match(ffsall$Survey_Id,table2$Survey_Id)
ffsall <- data.frame(ffsall, LanceEggLive=table2[getSLprop,13]) 

SLliveMos<-tapply(ffsall$LanceEggLive,ffsall$Month_,mean,na.rm=T)
barplot(SLliveMos)                         

savePlot(paste(pldir,script,"\\SLliveMos.pdf",sep=""),type="pdf")


SLliveType<-tapply(ffsall$LanceEggLive,ffsall$BeachType,mean,na.rm=T)
barplot(SLliveType)
savePlot(paste(pldir,script,"\\SLliveType.pdf",sep=""),type="pdf")

typeLive <- cbind(ssliveType,SLliveType)
write.csv(typeLive,paste(pldir,script,"/typeLive.csv",sep=""))
mosLive <- cbind(ssliveMos,SLliveMos)
write.csv(mosLive,paste(pldir,script,"/mosLive.csv",sep=""))

SLliveMosNS<-aggregate(ffsall$LanceEggLive,ffsall[c(28,15)],mean,na.rm=T)
ssliveMosNS<-aggregate(ffsall$SmeltEggsPercLive,ffsall[c(28,15)],mean,na.rm=T)

mosLiveNS <- cbind(ssliveMosNS,SLliveMosNS)
write.csv(mosLiveNS,paste(pldir,script,"/mosLiveNS.csv",sep=""))

lsept <- subset(ffsall,ffsall$Month_ == 9)
lsept[!is.na(lsept$LanceEggLive),]

loct <- subset(ffsall,ffsall$Month_ == 3)
loct[!is.na(loct$LanceEggLive),]

