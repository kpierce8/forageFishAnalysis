library(rpart)

ffenv3<- read.csv("g:/foragefish/ffenv2.csv")
ffenv3 <- ffenv3[!is.na(ffenv3$Fetch),]
Response <- ffenv3$lancesum

Response <- as.factor(ifelse(ffenv3$lancesum >0,  ffenv3$lancesum^0, 0))

Predictors <- ffenv3[,c(12:63,66:73,76:83)]

#Validation <- wavalid$VEGCLASS
#VPredictors <- wavalid.sp[,c(2:22,29:31,47:52,59:61,71:79)]
#

####    FIT MODEL    <<<<<<<<<
## method "class" or "anova" p258 MASS

cartdata = data.frame(Response,Predictors)
#validdata =  data.frame(Validation,VPredictors)
treefit = rpart(Response ~ .,data = cartdata,method="class", cp=.001)
printcp(treefit)
plotcp(treefit)
bob=table(cartdata[,1])

lance.prune <- prune(treefit, cp=.01)
plot(lance.prune)
text(lance.prune)


library(randomForest)

rflance <- randomForest(Predictors, Response, ntree=2000, na.action=na.omit)
#14 minutes run for 2000 trees with 3659 rows and 68 predictors
print(rflance)
importance(rflance)
varImpPlot(rflance, sort=TRUE)

rflance.nXY <- randomForest(Predictors[,c(-48,-49)], Response, ntree=2000, na.action=na.omit)
varImpPlot(rflance.nXY, sort=TRUE)