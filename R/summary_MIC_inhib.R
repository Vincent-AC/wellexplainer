#' Summarize MIC results
#'
#' Given OD MIC data processed through calculate_MIC_OD_inhib() function creates
#' a data.frame with the MIC for each row and the row identifier (first element
#' returned) and a list of data.frames splitting the first data.frames by strain
#' (second element returned)
#'
#' @param inhib_data_list A list of data.frames returned by the calculate_MIC_OD_inhib() function
#' @param number_of_lines Number of lines used by plate. Defaults to 8
#' @param species Species of the studied bacteria, for the moment only "Acinetobacter"
#' is available. Defaults to "Acinetobacter"
#' @keywords summary, MIC, results
#' @export
#' @examples
#' summary_MIC_inhib()
summary_MIC_inhib <- function(inhib_data_list,
                              number_of_lines = 8,
                              species = "Acinetobacter") {
  result_summary <- data.frame()
  strain_names <- list()
  for (i in 1:length(inhib_data_list))
  {
    inhib_data <- inhib_data_list[[i]]
    strain_names[[i]] <-
      strsplit(names(inhib_data_list)[i], "-")[[1]][1]
    result_summary[(i * number_of_lines - (number_of_lines - 1)):(i * number_of_lines), 1] <-
      strain_names[[i]]
    result_summary[(i * number_of_lines - (number_of_lines - 1)):(i * number_of_lines), 2] <-
      inhib_data[, "ATB"]
    result_summary[(i * number_of_lines - (number_of_lines - 1)):(i * number_of_lines), 3] <-
      inhib_data[, "ATB"]
    result_summary[(i * number_of_lines - (number_of_lines - 1)):(i * number_of_lines), 4] <-
      inhib_data[, "MIC"]

  }
  result_summary_final <- result_summary
  for (m in 1:nrow(result_summary))
  {
    result_summary_final[m, 2] <-
      strsplit(result_summary[, 2], "-")[[m]][1]
    result_summary_final[m, 3] <-
      as.numeric(strsplit(result_summary[, 2], "-")[[m]][2])
  }

  names(result_summary_final) <-
    c("Strain", "ATB", "Replicate", "MIC")

  list_of_strains <- unique(result_summary_final$"Strain")
  list_of_atb <- unique(result_summary_final$"ATB")
  results_by_strain <- list()
  for (n in 1:length(list_of_strains))
  {
    result_summary_strain <- data.frame()
    for (o in 1:length(list_of_atb))
    {
      result_summary_strain[o, 1] <-
        result_summary_final[result_summary_final$"Strain" == list_of_strains[n] &
                               result_summary_final$"ATB" == list_of_atb[o], "ATB"][1]
      result_summary_strain[o, 2] <-
        paste0(result_summary_final[result_summary_final$"Strain" == list_of_strains[n] &
                                      result_summary_final$"ATB" == list_of_atb[o], "MIC"][1], " - ",
               result_summary_final[result_summary_final$"Strain" ==
                                      list_of_strains[n] &
                                      result_summary_final$"ATB" == list_of_atb[o], "MIC"][2])
    }
    result_summary_strain[, 1] <-
      abbreviation_to_full_atb_name(result_summary_strain[, 1]) #remove abbreviations of atb names
    names(result_summary_strain) <- c("ATB", "MIC")
    result_summary_strain <-
      result_summary_strain[order(result_summary_strain[, 1]),]
    result_summary_strain <- clsi_breakpoints(result_summary_strain,separator = " - ",
                                                species = "Acinetobacter",
                                                MIC.column = 2,
                                                ATB.column = 1)
    results_by_strain[[n]] <- result_summary_strain
    names(results_by_strain)[n] <- list_of_strains[n]
    results_by_strain[[n]] <- result_summary_strain
    names(results_by_strain)[n] <- list_of_strains[n]
  }
  result_summary_final[, 2] <-
    abbreviation_to_full_atb_name(result_summary_final[, 2])
  result_summary_final <-
    result_summary_final[order(result_summary_final[, 1], result_summary_final[, 2]),]
  result_summary_final <- clsi_breakpoints(result_summary_final,separator = " - ",
                                              species = "Acinetobacter",
                                              MIC.column = 4,
                                              ATB.column = 2)
  results <-
    list(result_summary = result_summary_final,
         result_summary_by_strain = results_by_strain)
  return(results)
}
