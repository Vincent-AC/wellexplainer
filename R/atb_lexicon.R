#' Replace antibiotic abbreviations by full name
#'
#' This function takes a vector of characters, and replace the antibiotic abbreviations
#' by their full name 
#' 
#' @param string_to_change Vector containing the antibiotic abbreviations
#' @param language Target manguage of the full antibiotic names, "English" and "French" 
#' are available. Defaults to "English"
#' @keywords antibiotic names, abbreviations
#' @export
#' @examples
#' abbreviation_to_full_atb_name()
abbreviation_to_full_atb_name <-
  function(string_to_change, language = "English") {
    library(dplyr)
    lexicon <- data.frame(matrix(ncol = 3, nrow = 13))
    lexicon[, 1] <-
      c("tei",
        "chl",
        "min",
        "van",
        "rif",
        "tem",
        "cip",
        "mer",
        "fos",
        "pmb",
        "amk",
        "lin",
        "atm")
    lexicon[, 2] <-
      c(
        "Teicoplanin",
        "Chloramphenicol",
        "Minocycline",
        "Vancomycin",
        "Rifampicin",
        "Temocillin",
        "Ciprofloxacin",
        "Meropenem",
        "Fosfomycin",
        "Polymyxin B",
        "Amikacin",
        "Linezolid",
        "Aztreonam"
      )
    lexicon[, 3] <-
      c(
        "Teicoplanine",
        "Chloramphenicol",
        "Minocycline",
        "Vancomycine",
        "Rifampicine",
        "Temocilline",
        "Ciprofloxacine",
        "Meropeneme",
        "Fosfomycine",
        "Polymyxine B",
        "Amikacine",
        "Linezolide",
        "Aztreonam"
      )
    
    names(lexicon) <- c("Abbreviation", "English", "French")
    
    if (language == "English")
      language_id <- 2
    if (language == "French")
      language_id <- 3
    
    for (i in 1:nrow(lexicon))
    {
      string_to_change <-
        gsub(paste0("\\b",lexicon[i, 1],"\\b"), lexicon[i, language_id], string_to_change)
    }
    return(string_to_change)
  }