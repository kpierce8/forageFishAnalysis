library(rpart)
library(randomForest)
memory.limit(1024*3.75)
shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\RFsmelt", sep=""))
ffenv3<- read.csv("e:/foragefish/ffenv2.csv")

#Response <- ifelse(ffenv3$smeltsum >0,  ffenv3$smeltsum^0, 0)

ffenv3 <- ffenv3[!is.na(ffenv3$Fetch),]
ffenv3 <- ffenv3[!is.na(ffenv3$ABdist),]
dropcol <- match(c("UNIT_ID", "Id", "Id.1","Id.2","File_ID.1","File_ID","File_ID.2", "Shape", "Shape.1", "Shape.2","OBJECTID", "OBJECTID.1", "OBJECTID.2"), names(ffenv3))
Predictors<- ffenv3[,-dropcol]; Predictors<- Predictors[,c(11:dim(Predictors)[2])]
ni(Predictors)
# <- ffenv3[,c(12:63,68:77,82:91,96:105)]
Response <- ffenv3$smeltsum
Response <- as.factor(ifelse(ffenv3$smeltsum >0,  ffenv3$smeltsum^0, 0))
SMPercent <- ffenv3$smeltprop

####    FIT MODEL    <<<<<<<<<


rfsmelt <- randomForest(Predictors, Response, ntree=1000)
#14 minutes run for 2000 trees with 3659 rows and 68 predictors  (regression run)
print(rfsmelt)
importance(rfsmelt)
varImpPlot(rfsmelt, sort=TRUE)
savePlot(paste(pldir,"\\RFsmelt\\VarImp",proc.time()[3],".jpg",sep=""),type="jpg")

#partialPlot(rfsmelt,Predictors,MidY)
#savePlot(paste(pldir,"\\RFsmelt\\MidYDep",sep=""),type="jpg")
#partialPlot(rfsmelt,Predictors,MidX)
#savePlot(paste(pldir,"\\RFsmelt\\MidXDep",sep=""),type="jpg")
#

rfsmelt.nXY <- randomForest(Predictors[,c(-48,-49)], Response, ntree=1000)
print(rfsmelt.nXY)
varImpPlot(rfsmelt.nXY, sort=TRUE)
savePlot(paste(pldir,"\\RFsmelt\\VarImpnXY",proc.time()[3],".jpg",sep=""),type="jpg")

rfsmeltperc <- randomForest(Predictors, SMPercent, ntree=1000)
#14 minutes run for 2000 trees with 3659 rows and 68 predictors  (regression run)
print(rfsmelt)
importance(rfsmeltperc)
varImpPlot(rfsmeltperc, sort=TRUE)
savePlot(paste(pldir,"\\RFsmelt\\VarImpPerc",proc.time()[3],".jpg",sep=""),type="jpg")




## method "class" or "anova" p258 MASS

#cartdata = data.frame(Response,Predictors)
##validdata =  data.frame(Validation,VPredictors)
treefit = rpart(Response ~ .,data = Predictors[,c(-49,-50)],method="class", cp=.001)
printcp(treefit)
plotcp(treefit)


#bob=table(cartdata[,1])
#
smelt.prune <- prune(treefit, cp=.005)
plot(smelt.prune)
text(smelt.prune)
plotcp(smelt.prune)
#