#' Generate a PDF with CB raw results
#'
#' Generate a PDF with CB raw results, with each plate and CB results
#'
#' @param title Title of the PDF file
#' @param data_file Path to the output from the plate reader
#' @param knit_root_dir Directory where the report should be knitted (using knitr)
#' Defaults to the current working directory
#' @param output_dir Directory where the PDF should be saved. Defaults to the current working directory
#' @param max.conc.A vector giving the maximum concentration of
#' antibiotic A.The first element the vector
#' corresponds to the max concentration of antibiotic A for the first sheet.
#' @param max.conc.B vector giving the maximum concentration of
#' antibiotic B.The first element the vector
#' corresponds to the max concentration of antibiotic B for the first sheet.
#' @inheritParams calculate_CB_OD_growth
#' @keywords Report, CB, microdilution
#' @export
#' @examples
#' generate_CB_report()
generate_CB_report <-
  function(title,
           date,
           max.conc.A,
           max.conc.B,
           data_file,
           species,
           negative_control_col = 12,
           positive_control_col = 11,
           highest_conc_col.A = 1,
           lowest_conc_col.A = 10,
           highest_conc_row.B = 8,
           lowest_conc_row.B = 1,
           number_of_lines = 8,
           startRow = 24,
           endRow = 32,
           knit_root_dir = getwd(),
           output_dir = getwd())
  {
    library(rmarkdown)
    render(
      system.file("templates/CB_raw_results.R", package = "wellexplainer"),
      knit_root_dir = knit_root_dir,
      output_dir = output_dir,
      output_file=paste0("CB_", date, "_raw_results.pdf"),
      params = list(
        set_title = title,
        set_date = date,
        data_file = data_file,
        max.conc.A = max.conc.A,
        max.conc.B = max.conc.B,
        species = species,
        negative_control_col = negative_control_col,
        positive_control_col = positive_control_col,
        highest_conc_col.A = highest_conc_col.A,
        lowest_conc_col.A = lowest_conc_col.A,
        highest_conc_row.B = highest_conc_row.B,
        lowest_conc_row.B = lowest_conc_row.B,
        number_of_lines = number_of_lines,
        startRow = startRow,
        endRow = endRow
      ),
      encoding = "UTF-8"
    )
  }
