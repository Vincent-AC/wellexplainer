#' Extract data from spectrophotometer output
#'
#' This function allows you to extract optical density (OD) data from raw
#' output given by the TECAN OD reader, and store it to a list of
#' data.frame() (one data.frame per excel sheet) 
#' 
#' @param raw_data_filename path to the excel file
#' @param max.conc list of vector giving the maximum concentration of
#' antibiotic used for each row of each plate. The first element of the list
#' corresponds to the first excel sheet. The first element of each vector
#' corresponds to the first line of the plate
#' @param startRow Excel row number where the OD readings start
#' @param endRow Excel row number where the OD readings stop
#' @keywords MIC, Optical density
#' @export
#' @examples
#' extract_MIC_OD_data("../../CahierDeLabo/MyELN/JPIAMR-Acineto/MIC/
#' MIC_26-09-17/MIC-26-09-17.xlsx",
#' list(c(rep(512,4),rep(128,4)),512,512,512,512,512))

extract_MIC_OD_data <-
  function(raw_data_filename,
           max.conc,startRow = 24,
           endRow = 32)
  {
    library(xlsx)
    library(ggplot2)
    library(reshape2)
    
    workbook <- loadWorkbook(raw_data_filename)
    sheetnames <- getSheets(workbook)
    results <- NULL
    data_list <- list()
    plot_list <- list()
    strain_names <- list()
    result_summary <- data.frame()
    
    
    for (i in 1:length(sheetnames))
    {
      data_list[[i]] <-     read.xlsx(raw_data_filename,
                                      i,
                                      startRow = startRow,
                                      endRow = endRow)
      names(data_list)[i] <- names(sheetnames)[i]
      atb_names <- paste0(names(sheetnames)[i])
      atb_names <- strsplit(atb_names, "-")
      strain_names[[i]] <- atb_names[[1]][1]
      dataframe <- data_list[[i]]
      dataframe[, 1] <- "a"
      
      colnames(dataframe) <- c("ATB",1:12)
      rownames(dataframe) <- c("A","B","C","D","E","F","G","H")
      
      dataframe[, "Max.Conc"] <- max.conc[[i]]
      
      for (j in 1:4)
      {
        dataframe[2 * j - 1, 1] <-
          as.character(paste0(atb_names[[1]][j + 1], "-1"))
        dataframe[2 * j, 1] <-
          as.character(paste0(atb_names[[1]][j + 1], "-2"))
      }
    data_list[[i]]<-dataframe  
    }
return(data_list)
  }

