#'---
#'output:
#'  pdf_document:
#'    keep_tex: yes
#'    latex_engine: xelatex
#'params:
#'  set_title: "My Title!"
#'  set_date: "My Date!"
#'  data_file: "test"
#'  max.conc.A: "test"
#'  max.conc.B: "test"
#'  species: "test"
#'  negative_control_col: "test"
#'  positive_control_col: "test"
#'  highest_conc_col.A: "test"
#'  lowest_conc_col.A: "test"
#'  highest_conc_row.B: "test"
#'  lowest_conc_row.B: "test"
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

data <- extract_CB_OD_data(
  params$data_file,
  params$max.conc.A,
  params$max.conc.B,
  params$startRow,
  params$endRow
)

growth <- calculate_CB_OD_growth(data,
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

#+ Strain by strain results, echo = F, results='asis'
for (i in 1:length(results_by_strain))
{
  stargazer(
    results_by_strain[[i]],
    header = F,
    summary = F,
    digit.separator = " ",
    digits=2,
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
