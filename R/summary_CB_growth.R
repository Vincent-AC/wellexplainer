#' Summarize CB results
#'
#' Given OD MIC data processed through calculate_CB_OD_growth() function creates
#' a data.frame with the CB results for each plate and a list of data.frames splitting the first data.frames by strain
#' (second element returned)
#'
#' @param growth_data_list A list of data.frames returned by the calculate_CB_OD_growth() function
#' @param number_of_lines Number of lines used by plate. Defaults to 8
#' @param species Species of the studied bacteria, for the moment only "Acinetobacter"
#' is available. Defaults to "Acinetobacter"
#' @keywords summary, MIC, results
#' @export
#' @examples
#' summary_CB_growth()
summary_CB_growth <- function(growth_data_list,
                              number_of_lines = 8,
                              species = "Acinetobacter") {
  result_summary <- data.frame()
  strain_names <- list()
  for (i in 1:length(growth_data_list))
  {
    growth_data <- growth_data_list[[i]]
    strain_names[[i]] <-
      strsplit(names(growth_data_list)[i], "-")[[1]][1]
    result_summary[i, 1] <-
      strain_names[[i]]
    result_summary[i, 2] <-
      growth_data[1, "ATB.A"]
    result_summary[i, 3] <-
      growth_data[1, "ATB.B"]
    result_summary[i, 4] <-
      growth_data[1,"global.MIC.A"]
    result_summary[i, 5] <-
      growth_data[1,"global.MIC.B"]
    result_summary[i, 6] <-
      min(growth_data[-1,"min.FICI"],na.rm=T)
    result_summary[i, 7] <-
      growth_data[(growth_data[-1,"min.FICI"]==min(growth_data[-1,"min.FICI"],na.rm=T)),"MIC.A"][1]
    result_summary[i, 8] <-
      growth_data[(growth_data[-1,"min.FICI"]==min(growth_data[-1,"min.FICI"],na.rm=T)),"MIC.B"][1]

  }
  result_summary_final <- result_summary
  # for (m in 1:nrow(result_summary))
  # {
  #   result_summary_final[m, 2] <-
  #     strsplit(result_summary[, 2], "-")[[m]][1]
  #   result_summary_final[m, 3] <-
  #     as.numeric(strsplit(result_summary[, 2], "-")[[m]][2])
  # }

  names(result_summary_final) <-
    c("Strain", "ATB.A", "ATB.B", "MIC.A","MIC.B","FICI-min","Conc.A.FICI","Conc.B.FICI")

  list_of_strains <- unique(result_summary_final$"Strain")
  results_by_strain <- list()
  for (n in 1:length(list_of_strains))
  {
    result_summary_strain <- data.frame()
    result_summary_strain <- result_summary_final[result_summary_final$"Strain" == list_of_strains[n],]
    result_summary_strain <- result_summary_strain[,-1]

    result_summary_strain[, 1] <-
      abbreviation_to_full_atb_name(result_summary_strain[, 1])
    result_summary_strain[, 2] <-
      abbreviation_to_full_atb_name(result_summary_strain[, 2])#remove abbreviations of atb names
    result_summary_strain <-
      result_summary_strain[order(result_summary_strain[, 1]),]
    result_summary_strain <- CB_clsi_breakpoints(result_summary_strain,
                                              species = "Acinetobacter",
                                              MIC.A.column = 3,
                                              MIC.B.column = 4,
                                              FICI.conc.A.column=6,
                                              FICI.conc.B.column=7,
                                              ATB.A.column = 1,
                                              ATB.B.column = 2)
    results_by_strain[[n]] <- result_summary_strain
    names(results_by_strain)[n] <- list_of_strains[n]
  }
  result_summary_final[, 2] <-
    abbreviation_to_full_atb_name(result_summary_final[, 2])
  result_summary_final[, 3] <-
    abbreviation_to_full_atb_name(result_summary_final[, 3])
  result_summary_final <-
    result_summary_final[order(result_summary_final[, 1], result_summary_final[, 2],result_summary_final[, 3]),]
  result_summary_final <- CB_clsi_breakpoints(result_summary_final,
                                           species = "Acinetobacter",
                                           MIC.A.column = 4,
                                           MIC.B.column = 5,
                                           FICI.conc.A.column=7,
                                           FICI.conc.B.column=8,
                                           ATB.A.column = 2,
                                           ATB.B.column = 3)

  results <-
    list(result_summary = result_summary_final,
         result_summary_by_strain = results_by_strain)
  return(results)
}
