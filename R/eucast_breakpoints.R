#' Determine EUCAST sensitivity
#'
#' This function given a data.frame with MIC will add a column to this table 
#' giving the corresponding EUCAST sensitivity (Sensitive, Intermediate or Resistant)
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
#' eucast_breakpoints()
eucast_breakpoints <- function(MIC.table,separator=" - ",species = "Acinetobacter",
                               MIC.column=2,ATB.column=1) {

  if (species == "Acinetobacter")
  {
EUCAST.sensitivity <- NULL
    result <- MIC.table
    result[, (length(MIC.table)+1)] <- "a"
    for (i in 1:nrow(MIC.table))
    {
      if (sum(eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column]) == 0)
      {
        result[i, (length(MIC.table)+1)] <- "Antibiotic not found in EUCAST tables"
      }
      else if (is.na(eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 2]))
      {
        result[i, (length(MIC.table)+1)] <- "No cutoff set by EUCAST"
      }
      else
      {
        MIC.value <-
          unlist(strsplit(unlist(MIC.table[i, MIC.column], use.names = F), separator))
        for (j in 1:length(MIC.value))
        {

          if (sum(MIC.value[j] %in% c(
            "Positive control problem",
            "Negative control problem"
          )) != 0)
          {
            EUCAST.sensitivity[j] <- "No MIC value"
          }
          else
          {
            MIC.value <-
              as.numeric(unlist(strsplit(unlist(
                MIC.table[i, MIC.column], use.names = F
              ), separator)))
            if (MIC.value[j] <= eucast.breakpoints.acinetobacter[(eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column]), 2])
            {
              EUCAST.sensitivity[j] <- "Sensitive"
              
            }
            else if (MIC.value[j] > eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 3])
            {
              EUCAST.sensitivity[j] <- "Resistant"
            }
            else if (MIC.value[j] <= eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 3] &
                     MIC.value[j] > eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 2])
            {
              EUCAST.sensitivity[j] <- "Intermediate"
            }
          }
        }
        result[i, (length(MIC.table)+1)] <- paste0(EUCAST.sensitivity, collapse = separator)
      }
    }
    
  }
  colnames(result)[(length(MIC.table)+1)] <- "EUCAST Sensitivity"
  return(result)
}
