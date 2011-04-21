
newlat <-as.numeric(as.vector(beaches$Latdeg))+((as.numeric(as.vector(beaches$Latmin)) + ((as.numeric(beaches$Latsec))/100))/60)

newlong<- -1 * as.numeric(as.vector(beaches$Longdeg))-((as.numeric(as.vector(beaches$Longmin)) - ((as.numeric(beaches$Longsec))/100))/60)

bcoords <- data.frame(newlat, newlong)

write.csv(bcoords, "c:/data/projectslocal/forage_fish/bcoords6.csv")