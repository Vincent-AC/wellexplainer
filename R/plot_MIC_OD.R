#' Generate plots from OD results
#'
#' This function generates a plot for each plate read by OD. Each well is represented 
#' and the higher the OD is the darker the well will be represented
#' 
#' @param processed_OD_data_list A list of data.frames as produced by the extract_MIC_OD_data() function
#' @keywords plots, MIC, OD
#' @export
#' @examples
#' plot_MIC_OD()
plot_MIC_OD <- function(processed_OD_data_list)
{
  library(ggplot2)
  library(reshape2)
  plot_list <- list()
  for (i in 1:length(processed_OD_data_list)) {
    melted_data <-
      melt(subset(processed_OD_data_list[[i]], select = -Max.Conc),
           id.vars = "ATB")
    plot_OD <-
      ggplot(data = melted_data, aes(x = variable, y = ATB)) +
      geom_tile(aes(fill = value), colour = "white") +
      scale_fill_gradient(low = "lightblue",
                          high = "steelblue",
                          name = "Optical density") +
      scale_x_discrete(position = "top") +
      ggtitle(names(processed_OD_data_list)[i])
    
    plot_list[[i]] <- plot_OD
    names(plot_list)[i] <- paste0(names(processed_OD_data_list)[i], "_plot_OD")
  }
  return(plot_list)
}
