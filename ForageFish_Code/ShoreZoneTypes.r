## ShoreZOne analysis using BC types

##  szcounts is the main dataframe to work with

shell(paste("mkdir C:\\data\\rplots\\plottemp", gsub('-','',Sys.Date()),"\\SZ_types", sep=""))
script <- "\\SZ_types"




barplot(table(szcounts$BC_CLASS))

bc_samp <- table(szcounts$BC_CLASS)

bc_smelt <- table(szcounts[szcounts$smeltsum > -1,7])
bc_lance <- table(szcounts[szcounts$lancesum > -1,7])

barplot(bc_smelt,main="BC Smelt",col=c(2,3))
savePlot(paste(pldir,script,"\\BCtypes_Smelt.pdf",sep=""),type="pdf")

barplot(bc_lance,main="BC Lance",col=c(2,3))
savePlot(paste(pldir,script,"\\BCtypes_Lance.pdf",sep=""),type="pdf")

bc_smelt_pres <- table(szcounts[szcounts$smeltsum > 0,7])
bc_lance_pres <- table(szcounts[szcounts$lancesum > 0,7])

barplot(bc_smelt_pres,main="BC Smelt Presence",col=c(2,3))
savePlot(paste(pldir,script,"\\BCtypes_SmeltP.pdf",sep=""),type="pdf")

barplot(bc_lance_pres,main="BC Lance Presence",col=c(2,3))
savePlot(paste(pldir,script,"\\BCtypes_LanceP.pdf",sep=""),type="pdf")


bc_all <- table(szlinePF$BC_CLASS)
barplot(bc_all,main="BCtypes_all")
savePlot(paste(pldir,script,"\\BCtypes_all.pdf",sep=""),type="pdf")


bc_props <- matrix(0,33,7)
bc_props[,1] <- bc_all
bc_props[,2] <- bc_smelt
bc_props[match(names(bc_smelt_pres),names(bc_all)),3] <- bc_smelt_pres
bc_props[match(names(bc_lance_pres),names(bc_all)),4] <- bc_lance_pres
row.names(bc_props) <- names(bc_all)
bc_props[,5] <- bc_props[,2]/bc_props[,1]
bc_props[,6] <- bc_props[,3]/bc_props[,2]
bc_props[,7] <- bc_props[,4]/bc_props[,2]
