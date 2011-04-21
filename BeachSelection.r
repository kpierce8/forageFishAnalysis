
yrdata1972 <-ffsall[ffsall$Year_ %in% c(1972:1975),]
yrdata1982 <-ffsall[ffsall$Year_ %in% c(1980:1985),]
yrdata1992 <-ffsall[ffsall$Year_ %in% c(1992:1995),]
yrdata2002 <-ffsall[ffsall$Year_ %in% c(2001:2004),]

beach72<- unique(yrdata1972[,23])
beach82<- unique(yrdata1982[,23])
beach92<- unique(yrdata1992[,23])
beach02<- unique(yrdata2002[,23])

b92 <- beach92[beach92 %in% beach02]
b82 <- beach82[(beach82 %in% b92)]
b72 <- beach72[beach72 %in% b82]

yr0292 <- yrdata2002[yrdata2002[,23] %in% b92  ,]
yr9202 <- yrdata1992[yrdata1992[,23] %in% b92,]

