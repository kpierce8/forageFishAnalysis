# unit_data and szcounts are the primary data frames for this analysis.
# unit_data contains fish pa summaries for the 3689 beaches surveyed
# szcounts contains a subset of the szlines data plus the number of associated survey points

bematch <- match(unit_data[,1],szcounts[,1])
bematch <- bematch[!is.na(bematch)]
dim(szcounts[bematch,])

#beach_data <- data.frame(unit_data,szcounts[bmatch,])
beach_data <- szcounts[bematch,]
szNRDA <- table(szcounts$NRDA_CLASS)
survNRDA <- table(beach_data$NRDA_CLASS)

barplot(as.numeric(szNRDA),names.arg=row.names(szNRDA),main="NRDA class counts for szline and surveys")
barplot(as.numeric(survNRDA),names.arg=row.names(survNRDA),beside=TRUE,add=TRUE,col=2)

smelt.5 <- table(beach_data[beach_data$smeltprop > 0.5, 9])
barplot(as.numeric(survNRDA),names.arg=row.names(survNRDA),beside=TRUE,col=2)
barplot(as.numeric(smelt.5),names.arg=row.names(smelt.5),beside=FALSE,add=TRUE,col=3)

m1<- match(names(smelt.5),names(survNRDA))

smelt.ratio<-smelt.5/survNRDA[m1]

lance.5 <- table(beach_data[beach_data$lanceprop > 0.5, 9])
m2<- match(names(lance.5),names(survNRDA))

lance.ratio<-lance.5/survNRDA[m2]


plot(smelt.ratio)
plot(lance.ratio)

#ITZ_WIDTH
plot(beach_data$ITZ_WIDTH, beach_data$lanceprop, xlim=c(0,200))

plot(log(beach_data$ITZ_WIDTH), jitter(beach_data$lanceprop,2))

#RIPAR_PCT
plot(jitter(log(beach_data$RIPAR_PCT),10), jitter(beach_data$smeltprop,100),cex=.5)

plot(jitter(log(beach_data$RIPAR_PCT),10), jitter(beach_data$lanceprop,100),cex=.5)


gc<-get.continuous(beach_data)
beach_cont<-beach_data[,gc>0]
summary(beach_cont)

beach_cor <- data.frame(cor(beach_cont))

sig3(beach_cor[,39:42])


plot(jitter(beach_data$surveys,1),beach_data$smeltsum, cex=.5)
plot(log(beach_data$surveys),beach_data$smeltprop, cex=.5)
plot(beach_data$surveys,beach_data$smeltprop, cex=.5)

