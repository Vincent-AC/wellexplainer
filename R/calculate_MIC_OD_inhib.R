#' Convert OD into inhibition percentage
#'
#' Calculate inhibition percentage in each well in order to determine the MIC for each row
#' 
#' @param processed_OD_data_list List of data.frame. The first column of each
#' data.frame being the denomination of what is in the line and the following
#' columns being OD values read by the plate reader. The output of extract_MIC_OD_data
#' is already in the correct format.
#' @param negative_control_col Number of the column that contains the negative control.
#' Defaults to 12
#' @param positive_control_col Number of the column that contains the positive control.
#' Defaults to 11
#' @param higest_conc_col Number of the column that contains the highest concentration
#' of tested antibiotic. Defaults to 1
#' @param lowest_conc_col Number of the column that contains the lowest concentration
#' of tested antibiotic. Defaults to 10
#' @param number_of_lines Number of lines used by plate. Defaults to 8
#' @keywords MIC, microdilution, OD, inhibition
#' @export
#' @examples
#' calculate_MIC_OD_inhib()
calculate_MIC_OD_inhib <-
  function(processed_OD_data_list,
           negative_control_col = 12,
           positive_control_col = 11,
           highest_conc_col = 1,
           lowest_conc_col = 10,
           number_of_lines = 8,
           species)
  {
    inhib_data_list <- list()
    for (i in 1:length(processed_OD_data_list))
         {
           
           inhib_data <- processed_OD_data_list[[i]]
           inhib_data[, "MIC"] <- NULL
           dataframe <- processed_OD_data_list[[i]]
           
           for (l in 1:number_of_lines)
           {
             for (k in (highest_conc_col +1):(negative_control_col+1))
             {
               inhib_data[l, k] <- 1 -
                 ((dataframe[l, k] - dataframe[l, (negative_control_col+1)]) /
                  (dataframe[l, (positive_control_col+1)] - dataframe[l, (negative_control_col+1)]))
             }
             if (dataframe[l, (positive_control_col+1)] < 0.4)
             {
               inhib_data[l, "MIC"] <- "Positive control problem"
             }
             else if (dataframe[l, (negative_control_col+1)] > 0.4)
             {
               inhib_data[l, "MIC"] <- "Negative control problem"
             }
             else if (sum(inhib_data[l, (highest_conc_col +1):(lowest_conc_col +1)] >= 0.9) ==
                      0)
             {
               inhib_data[l, "MIC"] <- paste0(">", inhib_data[l, "Max.Conc"])
             }
             else if (sum(inhib_data[l, (highest_conc_col +1):(lowest_conc_col +1)] >= 0.9) ==
                      10)
             {
               inhib_data[l, "MIC"] <-
                 paste0("<=", min(geomSeries(2, inhib_data[l, "Max.Conc"])))
             }
             else if (dataframe[l, (positive_control_col+1)] >= 0.4 &
                      dataframe[l, (negative_control_col+1)] <= 0.4)
             {
               columns_without_growth <-
                 as.numeric(colnames(inhib_data[l,-c(1, (positive_control_col+1):ncol(inhib_data))])[which(inhib_data[l,-c(1, (positive_control_col+1):ncol(inhib_data))] >=
                                                                                                       0.9)])
               inhib_data[l, "MIC"] <-
                 min(geomSeries(2, inhib_data[l, "Max.Conc"])[columns_without_growth])
             }
           }
           inhib_data_list[[i]] <- inhib_data
           names(inhib_data_list)[i] <-
             paste0(names(processed_OD_data_list)[i],"_inhib")
           
    }
    return(inhib_data_list)
  }
