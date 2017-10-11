#' Generate a PDF with MIC raw results
#'
#' Generate a PDF with MIC raw results, with each plate and MIC results
#'
#' @param title Title of the PDF file
#' @param data_file Path to the output from the plate reader
#' @param knit_root_dir Directory where the report should be knitted (using knitr)
#' Defaults to the current working directory
#' @param output_dir Directory where the PDF should be saved. Defaults to the current working directory
#' @param max.conc list of vector giving the maximum concentration of
#' antibiotic used for each row of each plate. The first element of the list
#' corresponds to the first excel sheet. The first element of each vector
#' corresponds to the first line of the plates
#' @inheritParams calculate_MIC_OD_inhib
#' @keywords Report, MIC, microdilution
#' @export
#' @examples
#' generate_MIC_report()
generate_MIC_report <-
  function(title,
           date,
           max.conc,
           data_file,
           species,
           negative_control_col = 12,
           positive_control_col = 11,
           highest_conc_col = 1,
           lowest_conc_col = 10,
           number_of_lines = 8,
           startRow = 24,
           endRow = 32,
           knit_root_dir = getwd(),
           output_dir = getwd())
  {
    library(rmarkdown)
    render(
      system.file("templates/MIC_raw_results.R", package = "wellexplainer"),
      knit_root_dir = knit_root_dir,
      output_dir = output_dir,
      output_file=paste0("MIC_", date, "_raw_results.pdf"),
      params = list(
        set_title = title,
        set_date = date,
        data_file = data_file,
        max.conc = max.conc,
        species = species,
        negative_control_col = negative_control_col,
        positive_control_col = positive_control_col,
        highest_conc_col = highest_conc_col,
        lowest_conc_col = lowest_conc_col,
        number_of_lines = number_of_lines,
        startRow = startRow,
        endRow = endRow
      ),
      encoding = "UTF-8"
    )
  }