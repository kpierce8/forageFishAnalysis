# This is the script to import a point shapefile and create
# lines connecting all points

library(maptools)
library(rgdal)
library(shapefiles)
xx <- readShapePoints("N:\\ForageFish\\Pierce\\dist_test\\testpnts.shp")
ll <- read.shapefile("N:\\ForageFish\\Pierce\\dist_test\\testlines")


plot(xx)

#to call the first points coordinates
attributes(xx)$coords[1,]

#use sp package and its associated vignettes to build lines from scratch

numpts <- dim(xx)[1]

numlines <- sum(seq(1:numpts-1))

LinesHold <- new("list",seq(1:numlines))
nl <- 0

for(i in 2:numpts-1){
 nextpt <- i + 1
    for(j in nextpt:numpts){
point1 <- attributes(xx)$coords[i,]
point2 <- attributes(xx)$coords[j,]

l1 = rbind(point1,point2)
nl = nl+1
LinesHold[nl] = Line(l1)
     }
}
S1 <- Lines(LinesHold[1:nl], ID = "a")
SL <- SpatialLines(list(S1))

df = data.frame(z = c(1), row.names = sapply(slot(SL, "lines"), function(x) slot(x, "ID")))


Sldf = SpatialLinesDataFrame(SL, data = df)
summary(Sldf)



write.linelistShape(S1,df,"C:\\Rprojects\\spatialtest\\lines1")


write.shapefile(S1,"C:\\Rprojects\\spatialtest\\lines1")