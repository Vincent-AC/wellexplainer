#' Determine CLSI sensitivity
#'
#' This function given a data.frame with MIC will add a column to this table
#' giving the corresponding CLSI sensitivity (Sensitive, Intermediate or Resistant)
#'
#' @param MIC.table data.frame having MIC in one column. If multiple MIC are given
#' in one row they must be separated by separator
#' @param separator When multiple MIC are given in one cell they must be separated by
#' separator. Defaults to " - "
#' @param species Species of the studied bacteria, for the moment only "Acinetobacter"
#' is available. Defaults to "Acinetobacter"
#' @param MIC.column ID of the column containing the MIC values in MIC.table. Defaults to 2
#' @param ATB.column ID of the column containing the ATB names in MIC.table. Defaults to 1
#' @keywords placeholder
#' @export
#' @examples
#' clsi_breakpoints()
clsi_breakpoints <-
  function(MIC.table,
           separator = " - ",
           species = "Acinetobacter",
           MIC.column = 2,
           ATB.column = 1) {
    if (species == "Acinetobacter")
    {
      
      result <- MIC.table
      result[, (length(MIC.table) + 1)] <- "a"
      for (i in 1:nrow(MIC.table))
      {
        CLSI.sensitivity <- NULL
        if (sum(clsi.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column]) == 0)
        {
          result[i, (length(MIC.table) + 1)] <-
            "Antibiotic not found in CLSI tables"
        }
        else if (is.na(clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 2]))
        {
          result[i, (length(MIC.table) + 1)] <- "No cutoff set by CLSI"
        }
        else
        {
          MIC.value <-
            unlist(strsplit(unlist(MIC.table[i, MIC.column], use.names = F), separator))
          for (j in 1:length(MIC.value))
          {
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j]),
              is.na(MIC.value[j])
            ) > 0)
            {
              CLSI.sensitivity[j] <- "No MIC value"
            }
            
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j]),
              is.na(MIC.value[j])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(MIC.value[j])
              if (temp.MIC <= clsi.breakpoints.acinetobacter[(clsi.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column]), 2])
              {
                CLSI.sensitivity[j] <- "Sensitive"
                
              }
              else if (temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 3])
              {
                CLSI.sensitivity[j] <- "Resistant"
              }
              else if (temp.MIC <= clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 3] &
                       temp.MIC > clsi.breakpoints.acinetobacter[clsi.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 2])
              {
                CLSI.sensitivity[j] <- "Intermediate"
              }
            }
          }
          result[i, (length(MIC.table) + 1)] <-
            paste0(CLSI.sensitivity, collapse = separator)
        }
      }
      
    }
    colnames(result)[(length(MIC.table) + 1)] <-
      "CLSI Sensitivity"
    return(result)
  }
