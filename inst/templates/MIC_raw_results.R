#'---
#'output:
#'  pdf_document:
#'    keep_tex: yes
#'    latex_engine: xelatex
#'params:
#'  set_title: "My Title!"
#'  set_date: "My Date!"
#'  data_file: "test"
#'  max.conc: "test"
#'  species: "test"
#'  negative_control_col: "test"
#'  positive_control_col: "test"
#'  highest_conc_col: "test"
#'  lowest_conc_col: "test"
#'  number_of_lines: "test"
#'  startRow: "test"
#'  endRow: "test"
#'title: "`r params$set_title`"
#'date: "`r params$set_date` "
#'author: no 
#'---
#+ echo=F,warning=F,results='hide',error=F,message=FALSE
library("knitr")
library("stargazerCustom")
library("wellexplainer")

data <- extract_MIC_OD_data(
  params$data_file,
  params$max.conc,
  params$startRow,
  params$endRow
)

inhib <- calculate_MIC_OD_inhib(data,
                                params$negative_control_col,
                                params$positive_control_col,
                                params$highest_conc_col,
                                params$lowest_conc_col,
                                params$number_of_lines,
                                species=params$species)

plots <- plot_MIC_OD(data)

summary <- summary_MIC_inhib(inhib,
                             number_of_lines = params$number_of_lines,
                             species = params$species)
results_by_strain <- summary[[2]]

#+ Strain by strain results, echo = F, results='asis'
for (i in 1:length(results_by_strain))
{
  stargazer(
    results_by_strain[[i]],
    header = F,
    summary = F,
    digit.separator = " ",
    no.space = TRUE,
    notes = c("Concentrations en µg/mL"),
    rownames = F,
    title = paste0("Résultats ", names(results_by_strain)[i])
  )
}

#+ Plots, echo = F, results='asis'
for (j in 1:length(plots))
{
  print(plots[[j]])
}