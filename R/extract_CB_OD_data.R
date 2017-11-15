#' Extract data from spectrophotometer output
#'
#' This function allows you to extract optical density (OD) data from raw
#' output given by the TECAN OD reader, and store it to a list of
#' data.frame() (one data.frame per excel sheet) 
#' 
#' @param raw_data_filename path to the excel file
#' @param max.conc.A vector giving the maximum concentration of
#' antibiotic A.The first element the vector
#' corresponds to the max concentration of antibiotic A for the first sheet.
#' @param max.conc.B vector giving the maximum concentration of
#' antibiotic B.The first element the vector
#' corresponds to the max concentration of antibiotic B for the first sheet.
#' @param startRow Excel row number where the OD readings start
#' @param endRow Excel row number where the OD readings stop
#' @keywords CB, Optical density
#' @export
#' @examples
#' coucou

extract_CB_OD_data <-
  function(raw_data_filename,
           max.conc.A,
           max.conc.B,
           startRow = 24,
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
      dataframe[, 1] <- atb_names[[1]][2]
      dataframe[, "ATB.B"] <- atb_names[[1]][3]
      
      colnames(dataframe) <- c("ATB.A",1:12,"ATB.B")
      rownames(dataframe) <- c("A","B","C","D","E","F","G","H")
      
      dataframe <- dataframe[,c(1,14,2:13)]
      
      dataframe[, "Max.Conc.A"] <- max.conc.A[i]
      dataframe[, "Max.Conc.B"] <- max.conc.B[i]

      data_list[[i]]<-dataframe  
    }
    return(data_list)
  }

