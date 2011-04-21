library(RODBC)

GSU_tan <- odbcConnectAccess("e:/ForageFish/pierce/myRoutedFFDB.mdb")

#q2<- "SELECT * FROM PSU_midpoints"
q2<- "SELECT * FROM szMidUnique"
table1<-sqlQuery(GSU_tan,q2)

ni(table1)


# convert angle to radians  (USE BELOW TO FIX DIRECTION ERRORS)
#ps_rad <- (table1$LOC_ANGLE + table1$CorrectTan) / 180 * pi
ps_rad <- (table1$Fixed ) / 180 * pi
# calculate x and y offsets
xoff <- 100000 * cos(ps_rad)
yoff <- 100000 * sin(ps_rad)

# perform componenet addition

output <- c("Polyline")
for(i in 1:length(xoff)){
output<-c(output,paste(table1$UNIT_ID[i],0,sep=" "))
output<-c(output,paste(1,table1$MidX[i],table1$MidY[i], 0,0,sep=" "))
output<-c(output,paste(2,table1$MidX[i] + xoff[i],table1$MidY[i] + yoff[i],0,0,sep=" "))
}
output <- c(output,"END")

write.table(output,"e:/foragefish/szshore_orth.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)

#CreateFeaturesFromTextFile e:/foragefish/szshore_orth.txt . e:/foragefish/sz_orth #