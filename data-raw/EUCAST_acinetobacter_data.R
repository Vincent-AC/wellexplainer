library(xlsx)
eucast.breakpoints.acinetobacter <-
  read.xlsx("EUCAST_v_7.1_Breakpoint_Tables.xls", 8)

eucast.breakpoints.acinetobacter <-
  eucast.breakpoints.acinetobacter[4:130, 1:3]
row.names(eucast.breakpoints.acinetobacter) <-
  1:nrow(eucast.breakpoints.acinetobacter)

eucast.breakpoints.acinetobacter <-
  eucast.breakpoints.acinetobacter[-c(
    1,
    2,
    13,
    15,
    20,
    22:25,
    44:47,
    52:55,
    57:60,
    67:70,
    75:78,
    84:87,
    93,
    96:99,
    104:107,
    110:113,
    121
  ), ]

rownames(eucast.breakpoints.acinetobacter) <- NULL
colnames(eucast.breakpoints.acinetobacter) <-
  c("ATB", "S<=", "R>")
eucast.breakpoints.acinetobacter[, 1] <-
  as.character(eucast.breakpoints.acinetobacter[, 1])
eucast.breakpoints.acinetobacter[,1] <- gsub("[0-9]","",eucast.breakpoints.acinetobacter[,1])
eucast.breakpoints.acinetobacter[,2] <- as.numeric(gsub("[^0-9\\.]","",eucast.breakpoints.acinetobacter[,2]))
eucast.breakpoints.acinetobacter[,3] <- as.numeric(gsub("[^0-9\\.]","",eucast.breakpoints.acinetobacter[,3]))
devtools::use_data(eucast.breakpoints.acinetobacter,overwrite=T)
