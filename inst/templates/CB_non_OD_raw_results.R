#'---
#'output:
#'  pdf_document:
#'    keep_tex: yes
#'    latex_engine: xelatex
#'params:
#'  set_title: "My Title!"
#'  set_date: "My Date!"
#'  data_file: "test"
#'  species: "test"
#'  negative_control_col: "test"
#'  positive_control_col: "test"
#'  highest_conc_col.A: "test"
#'  lowest_conc_col.A: "test"
#'  highest_conc_row.B: "test"
#'  lowest_conc_row.B: "test"
#'  number_of_lines: "test"
#'title: "`r params$set_title`"
#'date: "`r params$set_date` "
#'author: no 
#'---
#+ echo=FALSE,warning=FALSE,results='hide',error=FALSE,message=FALSE
library("knitr")
library("stargazerCustom")
library("wellexplainer")
library("xlsx")
library("magrittr")


workbook <- loadWorkbook(params$data_file)
worksheets_names <- names(getSheets(workbook))

worksheets_names <- gsub("ATCC19606","ATCC19606-",worksheets_names)
worksheets_names <- gsub("Ab9","Ab9-",worksheets_names)
worksheets_names <- gsub("Ab186","Ab186-",worksheets_names)
worksheets_names <- gsub("CS01","CS01-",worksheets_names)
worksheets_names <- gsub("CR17","CR17-",worksheets_names)
worksheets_names <- gsub("Angers-160","Angers160",worksheets_names)%>%
  gsub("Angers160","Angers160-",.)
worksheets_names <- gsub("Angers-311","Angers311",worksheets_names)%>%
  gsub("Angers311","Angers311-",.)


FICI_present <- grepl(x=worksheets_names,pattern="FICI")

settings_present <- grepl(x=worksheets_names,pattern="settings")

sheets_with_raw_results <- !(FICI_present | settings_present)

sheets_with_raw_results_ID <- which(sheets_with_raw_results)

raw_results_list <- list()

for (i in 1:length(worksheets_names))
{
  if (i %in% sheets_with_raw_results_ID)
  {
    dataset <- read.xlsx(params$data_file, i)
    atb_a <- gsub("[0-9]","",colnames(dataset)[-1]) %>%
      gsub(" ","",.) %>%
      gsub("\\.","",.) 
    atb_b <- gsub("[0-9]","",dataset[,1]) %>%
      gsub(" ","",.) %>%
      gsub("\\.","",.)
    dataset[,"ATB.A"] <- atb_a[1]
    dataset[,"ATB.B"] <- atb_b[1]
    dataset <- dataset[,-c(1)]
    dataset <- dataset[,c(11,12,1:10)]
    ## For all these plates, controls were removed from the excel file when good
    ## because i had to type the results manually. We are going to add good controls
    ## In order to conform to the new format
    dataset[,"11"] <- 1
    dataset[,"12"] <- 0
    colnames(dataset) <- c("ATB.A","ATB.B",1:12)
    rownames(dataset) <- c("A","B","C","D","E","F","G","H")
    
    settings <- read.xlsx(params$data_file, i-1)
    dataset[, "Max.Conc.A"] <- settings[, "Max.Conc.A"]
    dataset[, "Max.Conc.B"] <- settings[, "Max.Conc.B"]
    
    raw_results_list[[length(raw_results_list)+1]] <- dataset
    names(raw_results_list)[length(raw_results_list)] <- worksheets_names[i]
    
  }
}


growth <- calculate_CB_OD_growth(raw_results_list,
                                 params$negative_control_col,
                                 params$positive_control_col,
                                 params$highest_conc_col.A,
                                 params$lowest_conc_col.A,
                                 params$highest_conc_row.B,
                                 params$lowest_conc_row.B,
                                 params$number_of_lines)

plots <- plot_CB_OD(growth)

summary <- summary_CB_growth(growth,
                            number_of_lines = params$number_of_lines,
                            species = params$species)

results_by_strain <- summary[[2]]

#+ Strain by strain results, echo = FALSE, results='asis'
for (i in 1:length(results_by_strain))
{
  stargazer(
    results_by_strain[[i]],
    header = F,
    summary = F,
    digit.separator = " ",
    digits = 2,
    no.space = TRUE,
    notes = c("Concentrations en µg/mL"),
    rownames = F,
    title = paste0("Résultats ", names(results_by_strain)[i])
  )
}

#+ Plots, echo = FALSE, results='asis'
for (j in 1:length(plots))
{
  print(plots[[j]])
}