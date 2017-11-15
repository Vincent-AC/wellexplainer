library(readODS)
clsi.breakpoints.acinetobacter <-
  read_ods("CLSI-Acineto-Breakpoints.ods", 1,skip=1)

#Remove the ATB + Inhibitor because it messes up with the col type
clsi.breakpoints.acinetobacter <- clsi.breakpoints.acinetobacter[-c(1,2,3,23),]
rownames(clsi.breakpoints.acinetobacter) <- NULL
colnames(clsi.breakpoints.acinetobacter) <-
  c("ATB", "S<=", "R>")
clsi.breakpoints.acinetobacter[, 1] <-
  as.character(clsi.breakpoints.acinetobacter[, 1])
clsi.breakpoints.acinetobacter[,2] <- as.numeric(gsub("[^0-9\\.]","",clsi.breakpoints.acinetobacter[,2]))
clsi.breakpoints.acinetobacter[,3] <- as.numeric(gsub("[^0-9\\.]","",clsi.breakpoints.acinetobacter[,3]))
devtools::use_data(clsi.breakpoints.acinetobacter,overwrite=T)
