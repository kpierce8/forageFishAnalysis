##Aggregate Forage Fish data
library(RODBC)
ddir <- c("e")
FFS2 <- odbcConnectAccess(paste(ddir,":/ForageFish/myRoutedFFDB.mdb",sep=""))

szmidpoints <- sqlFetch(FFS2, "szMidUnique")
szto10m <- sqlFetch(FFS2, "distT010m")
szFetch <- sqlFetch(FFS2, "szFetchUnique")

shape500 <- sqlFetch(FFS2, "concavity500m")
shape5000 <- sqlFetch(FFS2, "concavity5km")
shape10k <- sqlFetch(FFS2, "concavity10km")
#concavity <- ifelse(shape500$LandPerc > 0.5, -1 * shape500$SMdist, shape500$SMdist)
#shape500 <- data.frame(shape500, concavity)
#concavity <- ifelse(shape5000$LandPerc > 0.5, -1 * shape5000$SMdist, shape5000$SMdist)
#shape5000 <- data.frame(shape5000, concavity)
#

dim(szlinePFsing) #UNIT_ID
dim(szmidpoints)  #UNIT_ID
dim(szto10m)   #File_ID
dim(szFetch)  #File_ID
dim(shape500) # File_ID
dim(shape5000) # File_ID

#szmid_col <- names(szmidpoints)[c(7,6, 10,  28:35, 39, 49,50, 53, 54, 56,  58:80, 83:90 ,107,108, 112)]

midnames<- read.table("E:\\ForageFish\\midVarList.txt")
colget<-match(midnames[,2],names(szmidpoints))


ff_env <- szmidpoints[,colget]




fget <- match(ff_env$UNIT_ID, szFetch$File_ID)
s10get <- match(ff_env$UNIT_ID, szto10m$File_ID)
b500get <- match(ff_env$UNIT_ID, shape500$File_ID)
b5000get <- match(ff_env$UNIT_ID, shape5000$File_ID)
b10kget <- match(ff_env$UNIT_ID, shape10k$File_ID)

#Retrive the sample data from the ShoreZone calcs
ffget <- match(ff_env$UNIT_ID, unit_data$OBJECT_ID)

nosamp <- !ff_env$UNIT_ID  %in% unit_data$OBJECT_ID

validation <- ff_env[nosamp,]

test <- data.frame(ff_env$UNIT_ID, szFetch$File_ID[fget], szto10m$File_ID[s10get], shape500$File_ID[b500get],shape5000$File_ID[b5000get],unit_data$OBJECT_ID[ffget])

summary(test)

bobn<-names(test)
bob2<-strsplit(bobn,'\\.')
names(test) <- c(bob2[[1]][1] ,bob2[[2]][1],bob2[[3]][1],bob2[[4]][1],bob2[[5]][1])
write.csv(test,"E:/foragefish/text3.csv", row.names=FALSE)


env_set <- data.frame(ff_env, Fetch=szFetch$Shape_Length[fget], SZ10m =szto10m$Shape_Length[s10get], SinTransform = sin(ff_env$Fixed/180*pi), shape500[b500get,],shape5000[b5000get,],shape5000[b10kget,])

plot(jitter(env_set$FET_NORM),env_set$Fetch,xlim=c(0,20),pch='.')
plot((env_set$FET_NORM),env_set$Fetch,xlim=c(0,20),pch='?')


unitget <- match(unit_data$OBJECT_ID, ff_env$UNIT_ID)

ffenvModel <- data.frame(unit_data, env_set[unitget,])

ffenv2 <- ffenvModel[!is.na(ffenvModel$FUC_UNIT),]
ffenv2 <- ffenvModel[!is.na(ffenvModel$Fetch),]
ffenv2 <- ffenvModel[!is.na(ffenvModel$SZ10m),]

write.csv(ffenv2,"e:/foragefish/ffenv2.csv")


todd_var <- c(12, 3:10,74,76,78,88,90,92,102,104,106)

-
-
 
newdata <- ffenv2[,todd_var -1 ]
 
 
write.dbf(newdata, "e:/foragefish/forTodd.dbf")
