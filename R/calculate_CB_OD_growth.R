#' Convert OD into growth percentage
#'
#' Calculate growth percentage in each well in order to determine the MIC of A and B for each row
#'
#' @param processed_OD_data_list List of data.frame. The first column of each
#' data.frame being the denomination of what is in the line and the following
#' columns being OD values read by the plate reader. The output of extract_MIC_OD_data
#' is already in the correct format.
#' @param negative_control_col Number of the column that contains the negative control.
#' Defaults to 12
#' @param positive_control_col Number of the column that contains the positive control.
#' Defaults to 11
#' @param higest_conc_col.A Number of the column that contains the highest concentration
#' of ATB.A. Defaults to 1
#' @param lowest_conc_col.A Number of the column that contains the lowest concentration
#' of ATB.A Defaults to 10
#' @param higest_conc_row.B Number of the row that contains the highest concentration
#' of ATB.B. Defaults to 8
#' @param lowest_conc_row.B Number of the row that contains the lowest concentration
#' of ATB.B Defaults to 1
#' @param number_of_lines Number of lines used by plate. Defaults to 8
#' @keywords CB, microdilution, OD, growth
#' @export
#' @examples
#' calculate_CB_OD_growth()
calculate_CB_OD_growth <-
  function(processed_OD_data_list,
           negative_control_col = 12,
           positive_control_col = 11,
           highest_conc_col.A = 1,
           lowest_conc_col.A = 10,
           highest_conc_row.B = 8,
           lowest_conc_row.B = 1,
           number_of_lines = 8)
  {
    growth_data_list <- list()
    for (i in 1:length(processed_OD_data_list))
    {
      growth_data <- processed_OD_data_list[[i]]
      growth_data[, "MIC.A"] <- NA
      growth_data[, "MIC.B"] <- NA
      growth_data[, "min.FICI"] <- NA
      growth_data[, "global.MIC.A"] <- NA
      growth_data[, "global.MIC.B"] <- NA
      dataframe <- processed_OD_data_list[[i]]
      
      
      for (l in 1:number_of_lines)
      {
        for (k in (highest_conc_col.A + 2):(negative_control_col + 2))
        {
          growth_data[l, k] <-
            ((dataframe[l, k] - dataframe[l, (negative_control_col + 2)]) /
               (dataframe[l, (positive_control_col + 2)] - dataframe[l, (negative_control_col +
                                                                           2)]))
        }
      }
      conc.B <-
        c(0, rev(geomSeries(2, growth_data[1, "Max.Conc.B"], n = number_of_lines -
                              1)))
      rows_wo_growth <-
        which(growth_data[, (lowest_conc_col.A + 2)] <= 0.1)
      
      
      for (l in 1:number_of_lines)
      {
        if (max(growth_data[l, (highest_conc_col.A + 2):(lowest_conc_col.A + 2)]) > 1.5)
          #If there is more than 50% growth in one of the wells that isn't the postiive control, consider the positive control as too low
        {
          growth_data[l, "MIC.A"] <- "Failed ctl+"
          growth_data[l, "MIC.B"] <- "Failed ctl+"
          growth_data[l, "min.FICI"] <- "Failed ctl+"
        }
        else if (dataframe[l, (negative_control_col + 2)] > 0.2)
        {
          growth_data[l, "MIC.A"] <- "Failed ctl-"
          growth_data[l, "MIC.B"] <- "Failed ctl-"
          growth_data[l, "min.FICI"] <- "Failed ctl-"
        }
        else if (sum(growth_data[l, (highest_conc_col.A + 2):(lowest_conc_col.A +
                                                              2)] <= 0.1) ==
                 0)
        {
          growth_data[l, "MIC.A"] <- paste0(">", growth_data[l, "Max.Conc.A"])
          growth_data[l, "MIC.B"] <-
            paste0(">", c(0, rev(
              geomSeries(2, growth_data[l, "Max.Conc.B"], n = number_of_lines - 1)
            ))[l])
        }
        else if (sum(growth_data[l, (highest_conc_col.A + 2):(lowest_conc_col.A +
                                                              2)] <= 0.1) ==
                 10 & l != 2)
        {
          growth_data[l, "MIC.A"] <-
            paste0("<=", min(geomSeries(2, growth_data[l, "Max.Conc.A"], 9)))
          growth_data[l, "MIC.B"] <-
            c(0, rev(geomSeries(2, growth_data[l, "Max.Conc.B"], n = number_of_lines -
                                  1)))[l]
        }
        else if (sum(growth_data[l, (highest_conc_col.A + 2):(lowest_conc_col.A +
                                                              2)] <= 0.1) ==
                 10 & l == 2)
        {
          growth_data[l, "MIC.A"] <-
            paste0("<=", min(geomSeries(2, growth_data[l, "Max.Conc.A"], 9)))
          growth_data[l, "MIC.B"] <- paste0("<=",
                                            c(0, rev(
                                              geomSeries(2, growth_data[l, "Max.Conc.B"], n = number_of_lines -
                                                           1)
                                            ))[l])
          
        }
        
        else if (max(growth_data[l, (highest_conc_col.A + 2):(lowest_conc_col.A +
                                                              2)]) <= 1.5 &
                 dataframe[l, (negative_control_col + 2)] <= 0.2)
        {
          columns_without_growth <-
            as.numeric(colnames(growth_data[l, -c(1, 2, (positive_control_col +
                                                           2):ncol(growth_data))])[which(growth_data[l, -c(1, 2, (positive_control_col +
                                                                                                                    2):ncol(growth_data))] <=
                                                                                           0.1)])
          growth_data[l, "MIC.A"] <-
            min(geomSeries(2, growth_data[l, "Max.Conc.A"], 9)[columns_without_growth])
          growth_data[l, "MIC.B"] <-
            c(0, rev(geomSeries(2, growth_data[l, "Max.Conc.B"], n = number_of_lines -
                                  1)))[l]
        }
        
      }
      
      global.MIC.A <- growth_data[lowest_conc_row.B, "MIC.A"]
      growth_data[, "global.MIC.A"] <- global.MIC.A
      global.MIC.B <- growth_data[min(rows_wo_growth), "MIC.B"]
      growth_data[, "global.MIC.B"] <- global.MIC.B
      growth_data[, "rows_wo_growth"] <- min(rows_wo_growth)      
      
      for (l in 1:number_of_lines)
      {
        if (!is.na(as.numeric(growth_data[l, "MIC.A"])) &
            !is.na(as.numeric(growth_data[l, "MIC.B"])) &
            !is.na(as.numeric(growth_data[l, "global.MIC.A"])) &
            !is.na(as.numeric(growth_data[l, "global.MIC.B"])))
        {
          FICI <-
            (
              as.numeric(growth_data[l, "MIC.A"]) / as.numeric(growth_data[l, "global.MIC.A"]) +
                as.numeric(growth_data[l, "MIC.B"]) / as.numeric(growth_data[l, "global.MIC.B"])
            )
          growth_data[l, "min.FICI"] <- FICI
        }
      }
      growth_data_list[[i]] <- growth_data
      names(growth_data_list)[i] <-
        paste0(names(processed_OD_data_list)[i], "_growth")
      
    }
    return(growth_data_list)
  }
