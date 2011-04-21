PSNERPconn <- odbcConnectAccess("E:/PSNERP/PS-PSNERP-CA-1.1.mdb")
sqltmp <- odbcConnectAccess2007("E:/PSNERP/temp.accdb")

psArmor <- sqlFetch(PSNERPconn, "all_Armoring")
psShore <- sqlFetch(PSNERPconn, "fd_shoreline_current")

beach_lengths <- tapply(psArmor$Shape_Length,psArmor$GSU_ID,sum)

psArmorY <- psArmor[psArmor$Armor_YN == c('Y'),]
head(psArmorY)


armor_lengths <- tapply(psArmorY$Shape_Length,psArmorY$GSU_ID,sum)
head(armor_lengths)

is.na(armor_lengths) <- 0
bob<-data.frame(armor=armor_lengths,beach=beach_lengths,percent=armor_lengths/beach_lengths)

garm <- match(ffsall$PSNERP_GSU,row.names(bob))

ffsall2 <- data.frame(ffsall,bob[garm,])
glength<-match(row.names(ff_armor_data),row.names(beach_lengths))

beach_length2 <- beach_lengths[glength]
length(beach_length2)


tgsu<- as.character(ffsall2$PSNERP_GSU)
outgsu <- c("")
i <- 1
for (i in 1:length(tgsu)){
temp <- strsplit(tgsu[i],"-")
temp2 <- paste(temp[[1]][1],temp[[1]][2],sep="-")
outgsu <- c(outgsu,temp2)
}

lance2[1:100,]

ffsall2 <- data.frame(ffsall2,outgsu[-1])
names(ffsall2)[38] <- c("outgsu")
beach_sum <- tapply(ffsall2$outgsu, ffsall2$outgsu,length)
beach_smelt <- tapply(ffsall2$Smelt_Ind, ffsall2$outgsu,sum,na.rm=T)
beach_lance <- tapply(ffsall2$Sand_Lance_Ind, ffsall2$outgsu,sum,na.rm=T)
beach_armor <- tapply(ffsall2$percent, ffsall2$outgsu,mean,na.rm=T)
beach_length <- tapply(ffsall2$beach, ffsall2$outgsu,mean,na.rm=T)

ff_armor_data <- data.frame(records=beach_sum,smelt=beach_smelt,lance=beach_lance,armor=beach_armor,beach_length=beach_length)

ff_armor_data[is.na(ff_armor_data$armor),4] <- 0

hist(ff_armor_data$armor)
hist(bob$percent)

