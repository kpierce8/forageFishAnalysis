#NOSC data
shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\Nosc", sep=""))
script <- "\\Nosc"

noscnn <- odbcConnectAccess2007("e:/foragefish/ff_sql_copy.mdb")

nosc <- sqlFetch(noscnn,"NOSC_data")

dim(nosc)

nosc_ind<-data.frame(tapply(nosc$Smelt_Ind,nosc[,13],sum,na.rm=T),tapply(nosc$Sand_Lance_Ind,nosc[,13],sum,na.rm=T),as.vector(table(nosc[,13])),as.vector(table(ffsall[,29])[-6]))
table(nosc[,13])
names(nosc_ind) <- c("Smelt","Lance","Nosc Sampled","All Sampled")

nosc_ind
nosc_ind <- data.frame(nosc_ind,SmeltP=sig3(nosc_ind[,1]/nosc_ind[,3]),LanceP=sig3(nosc_ind[,2]/nosc_ind[,3]))

write.csv(nosc_ind,paste(pldir,script,"/nosc_ind.csv",sep=""))

ffs_ind<-data.frame(tapply(ffsall$Smelt_Ind,ffsall[,29],sum,na.rm=T),tapply(ffsall$Sand_Lance_Ind,ffsall[,29],sum,na.rm=T),as.vector(table(ffsall[,29])))
names(ffs_ind) <- c("Smelt","Lance","All Sampled")
ffs_ind <- data.frame(ffs_ind,SmeltP=sig3(ffs_ind[,1]/ffs_ind[,3]),LanceP=sig3(ffs_ind[,2]/ffs_ind[,3]))
write.csv(ffs_ind,paste(pldir,script,"/ffs_ind.csv",sep=""))

nonosc <- read.dbf("e:/foragefish/nonnosc_sample.dbf")
nonosc_ind<-data.frame(tapply(nonosc$Smelt_Ind,nonosc[,28],sum,na.rm=T),tapply(nonosc$Sand_Lance,nonosc[,28],sum,na.rm=T),as.vector(table(nonosc[,28])))
names(nonosc_ind) <- c("Smelt","Lance","NonNosc Sampled")
nonosc_ind <- data.frame(nonosc_ind,SmeltP=sig3(nonosc_ind[,1]/nonosc_ind[,3]),LanceP=sig3(nonosc_ind[,2]/nonosc_ind[,3]))

write.csv(nonosc_ind,paste(pldir,script,"/nonosc_ind.csv",sep=""))


noscall <- read.dbf("e:/foragefish/nosc_samplesall.dbf")
nonosc <- read.dbf("e:/foragefish/nonnosc_sample.dbf")


