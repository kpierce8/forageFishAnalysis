library(spatstat)

eggsamps <- szlinePF[!is.na(szlinePF[,105]),c(4,104)]
eggzones <- szlinePF[!is.na(szlinePF[,105]),104]
eggbeach <- ffsall[match(eggsamps[,1],ffsall[,23]),]
eggbeach <- data.frame(eggbeach,region=eggsamps[match(eggbeach[,23],eggsamps[,1]),2])


samps <- as.ppp(eggbeach[,11:12],ripras(eggbeach[,11:12]))

plot(samps,pch=".")

summary(samps)

#bob <- ppm(samps, ~1, Strauss(3000))


i<- 1

eggsub <- eggbeach[eggbeach$region == i,]
samp1 <- sample(dim(eggsub)[1],30)
newsamp <- eggsub[samp1,]
oldsamp <- eggsub[setdiff(seq(1:dim(eggsub)[1]),samp1),]
bob<-as.matrix(dist(rbind(eggsub[48,11:12],newsamp[,11:12])))

if(min(bob[-1,1])< 3000) {
  delb <- which.min(bob[-1,1])
  temp1 <- c(temp1,delb)


  