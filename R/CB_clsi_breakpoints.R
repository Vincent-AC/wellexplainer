#' Determine CLSI sensitivity
#'
#' This function given a data.frame with MICs will add a column to this table
#' giving the corresponding CLSI sensitivity (Sensitive, Intermediate or Resistant)
#'
#' @param CB.table data.frame having MIC.A and MIC.B in one column respectively.
#' @param species Species of the studied bacteria, for the moment only "Acinetobacter"
#' is available. Defaults to "Acinetobacter"
#' @param MIC.A.column ID of the column containing the MIC values in CB.table. Defaults to 2
#' @param MIC.B.column ID of the column containing the MIC values in CB.table. Defaults to 2
#' @param ATB.A.column ID of the column containing the ATB names in CB.table. Defaults to 1
#' @param ATB.B.column ID of the column containing the ATB names in CB.table. Defaults to 1
#' @keywords placeholder
#' @export
#' @examples
#' CB_clsi_breakpoints()
CB_clsi_breakpoints <-
  function(CB.table,
           species = "Acinetobacter",
           MIC.A.column = 3,
           MIC.B.column = 4,
           FICI.conc.A.column=6,
           FICI.conc.B.column=7,
           ATB.A.column = 1,
           ATB.B.column = 2) {
    if (species == "Acinetobacter")
    {
      CLSI.sensitivity <- NULL
      result <- CB.table
      result[, (length(CB.table) + 1):(length(CB.table) + 4)] <- "a"
      for (i in 1:nrow(CB.table))
      {
        if (sum(clsi.breakpoints.acinetobacter[, 1] == CB.table[i, ATB.A.column]) == 0)
        {
          result[i, (length(CB.table) + 1)] <-
            "NA"
        }
        else if (is.na(clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[i, ATB.A.column], 2]))
        {
          result[i, (length(CB.table) + 1)] <- "NA"
        }
        else
        {
          MIC.A.value <-
            unlist(CB.table[i, MIC.A.column], use.names = F)
          for (j in 1:length(MIC.A.value))
          {
            if (sum(
              grepl("<=", MIC.A.value[j]),
              grepl(">", MIC.A.value[j]),
              grepl("Failed ctl+", MIC.A.value[j]),
              grepl("Failed ctl-", MIC.A.value[j]),
              is.na(MIC.A.value[j])
            ) > 0)
            {
              CLSI.sensitivity[j] <- "NA"
            }
            
            if (sum(
              grepl("<=", MIC.A.value[j]),
              grepl(">", MIC.A.value[j]),
              grepl("Failed ctl+", MIC.A.value[j]),
              grepl("Failed ctl-", MIC.A.value[j]),
              is.na(MIC.A.value[j])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(MIC.A.value[j])
              if (temp.MIC <= clsi.breakpoints.acinetobacter[(clsi.breakpoints.acinetobacter[, 1] == CB.table[i, ATB.A.column]), 2])
              {
                CLSI.sensitivity[j] <- "S"
                
              }
              else if (temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[i, ATB.A.column], 3])
              {
                CLSI.sensitivity[j] <- "R"
              }
              else if (temp.MIC <= clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[i, ATB.A.column], 3] &
                       temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[i, ATB.A.column], 2])
              {
                CLSI.sensitivity[j] <- "I"
              }
            }
          }
          result[i, (length(CB.table) + 1)] <- CLSI.sensitivity
        }
      }
      CLSI.sensitivity <- NULL
      for (l in 1:nrow(CB.table))
      {
        if (sum(clsi.breakpoints.acinetobacter[, 1] == CB.table[l, ATB.B.column]) == 0)
        {
          result[l, (length(CB.table) + 2)] <-
            "NA"
        }
        else if (is.na(clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[l, ATB.B.column], 2]))
        {
          result[l, (length(CB.table) + 2)] <- "NA"
        }
        else
        {
          MIC.B.value <-
            unlist(CB.table[l, MIC.B.column], use.names = F)
          for (m in 1:length(MIC.B.value))
          {
            if (sum(
              grepl("<=", MIC.B.value[m]),
              grepl(">", MIC.B.value[m]),
              grepl("Failed ctl+", MIC.B.value[m]),
              grepl("Failed ctl-", MIC.B.value[m]),
              is.na(MIC.B.value[m])
            ) > 0)
            {
              CLSI.sensitivity[m] <- "NA"
            }
            
            if (sum(
              grepl("<=", MIC.B.value[m]),
              grepl(">", MIC.B.value[m]),
              grepl("Failed ctl+", MIC.B.value[m]),
              grepl("Failed ctl-", MIC.B.value[m]),
              is.na(MIC.B.value[m])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(MIC.B.value[m])
              if (temp.MIC <= clsi.breakpoints.acinetobacter[(clsi.breakpoints.acinetobacter[, 1] == CB.table[l, ATB.B.column]), 2])
              {
                CLSI.sensitivity[m] <- "S"
                
              }
              else if (temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[l, ATB.B.column], 3])
              {
                CLSI.sensitivity[m] <- "R"
              }
              else if (temp.MIC <= clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[l, ATB.B.column], 3] &
                       temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[l, ATB.B.column], 2])
              {
                CLSI.sensitivity[m] <- "I"
              }
            }
          }
          result[l, (length(CB.table) + 2)] <- CLSI.sensitivity
        }
      }
      CLSI.sensitivity <- NULL
      for (n in 1:nrow(CB.table))
      {
        if (sum(clsi.breakpoints.acinetobacter[, 1] == CB.table[n, ATB.A.column]) == 0)
        {
          result[n, (length(CB.table) + 3)] <-
            "NA"
        }
        else if (is.na(clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[n, ATB.A.column], 2]))
        {
          result[n, (length(CB.table) + 3)] <- "NA"
        }
        else
        {
          FICI.conc.A.value <-
            unlist(CB.table[n, FICI.conc.A.column], use.names = F)
          for (o in 1:length(FICI.conc.A.value))
          {
            if (sum(
              grepl("<=", FICI.conc.A.value[o]),
              grepl(">", FICI.conc.A.value[o]),
              grepl("Failed ctl+", FICI.conc.A.value[o]),
              grepl("Failed ctl-", FICI.conc.A.value[o]),
              is.na(FICI.conc.A.value[o])
            ) > 0)
            {
              CLSI.sensitivity[o] <- "NA"
            }
            
            if (sum(
              grepl("<=", FICI.conc.A.value[o]),
              grepl(">", FICI.conc.A.value[o]),
              grepl("Failed ctl+", FICI.conc.A.value[o]),
              grepl("Failed ctl-", FICI.conc.A.value[o]),
              is.na(FICI.conc.A.value[o])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(FICI.conc.A.value[o])
              if (temp.MIC <= clsi.breakpoints.acinetobacter[(clsi.breakpoints.acinetobacter[, 1] == CB.table[n, ATB.A.column]), 2])
              {
                CLSI.sensitivity[o] <- "S"
                
              }
              else if (temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[n, ATB.A.column], 3])
              {
                CLSI.sensitivity[o] <- "R"
              }
              else if (temp.MIC <= clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[n, ATB.A.column], 3] &
                       temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[n, ATB.A.column], 2])
              {
                CLSI.sensitivity[o] <- "I"
              }
            }
          }
          result[n, (length(CB.table) + 3)] <- CLSI.sensitivity
        }
      }
      CLSI.sensitivity <- NULL
      for (p in 1:nrow(CB.table))
      {
        if (sum(clsi.breakpoints.acinetobacter[, 1] == CB.table[p, ATB.B.column]) == 0)
        {
          result[p, (length(CB.table) + 4)] <-
            "NA"
        }
        else if (is.na(clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[p, ATB.B.column], 2]))
        {
          result[p, (length(CB.table) + 4)] <- "NA"
        }
        else
        {
          FICI.conc.B.value <-
            unlist(CB.table[p, FICI.conc.B.column], use.names = F)
          for (q in 1:length(FICI.conc.B.value))
          {
            if (sum(
              grepl("<=", FICI.conc.B.value[q]),
              grepl(">", FICI.conc.B.value[q]),
              grepl("Failed ctl+", FICI.conc.B.value[q]),
              grepl("Failed ctl-", FICI.conc.B.value[q]),
              is.na(FICI.conc.B.value[q])
            ) > 0)
            {
              CLSI.sensitivity[q] <- "NA"
            }
            
            if (sum(
              grepl("<=", FICI.conc.B.value[q]),
              grepl(">", FICI.conc.B.value[q]),
              grepl("Failed ctl+", FICI.conc.B.value[q]),
              grepl("Failed ctl-", FICI.conc.B.value[q]),
              is.na(FICI.conc.B.value[q])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(FICI.conc.B.value[q])
              if (temp.MIC <= clsi.breakpoints.acinetobacter[(clsi.breakpoints.acinetobacter[, 1] == CB.table[p, ATB.B.column]), 2])
              {
                CLSI.sensitivity[q] <- "S"
                
              }
              else if (temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[p, ATB.B.column], 3])
              {
                CLSI.sensitivity[q] <- "R"
              }
              else if (temp.MIC <= clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[p, ATB.B.column], 3] &
                       temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == CB.table[p, ATB.B.column], 2])
              {
                CLSI.sensitivity[q] <- "I"
              }
            }
          }
          result[p, (length(CB.table) + 4)] <- CLSI.sensitivity
        }
      }
    }
    result[,(length(CB.table) + 1)] <- paste0(result[,(length(CB.table) + 1)],"->",result[,(length(CB.table) + 3)])
    result[,(length(CB.table) + 2)] <- paste0(result[,(length(CB.table) + 2)],"->",result[,(length(CB.table) + 4)])
    result <- result[,-c((length(CB.table) + 3),(length(CB.table) + 4))]
    colnames(result)[(length(CB.table) + 1)] <-
      "Sensi.A"
    colnames(result)[(length(CB.table) + 2)] <-
      "Sensi.B"
    return(result)
  }
