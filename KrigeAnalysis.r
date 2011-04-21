krigesamp <- read.dbf("N:\\ForageFish\\Pierce\\krigesample2.dbf")
smelt.t<-t.test(krigesamp[,2],krigesamp[,3])

lanceksamp<-krigesamp[krigesamp[,4] >0.0000,3:4]
lance.t<-t.test(lanceksamp[,1],lanceksamp[,2])

smelt.all <- density(krigesamp[,2])
smelt.samp<- density(krigesamp[,3])
plot(smelt.all, main="Density of Smelt beach sampling Cos(Mos)")
lines(smelt.samp,col=2)
legend(-.75,.75,"Black=all,Red=Samp")


lance.all <- density(lanceksamp[,1])
lance.samp<- density(lanceksamp[,2])

plot(lance.all,ylim=c(0,13),main="Density of Lance beach sampling Cos(Mos)")
lines(lance.samp,col=2)
legend(-.75,6,"Black=all,Red=Samp")
