#' Generate plots from OD results
#'
#' This function generates a plot for each plate read by OD. Each well is represented 
#' and the higher the growthition is the darker the well will be represented
#' 
#' @param processed_growth_data_list A list of data.frames as produced by the calculate_CB_OD_growth() function
#' @keywords plots, CB, OD, growthition
#' @export
#' @examples
#' plot_CB_OD()
#' 
plot_CB_OD <- function(processed_growth_data_list)
{
  library(ggplot2)
  library(reshape2)
  plot_list <- list()
  for (i in 1:length(processed_growth_data_list)) {
    data_to_melt <- subset(processed_growth_data_list[[i]],select=-c(ATB.A,ATB.B,Max.Conc.A,Max.Conc.B,MIC.A,MIC.B,min.FICI,global.MIC.A,global.MIC.B,rows_wo_growth))
    data_to_melt[,"ID"] <- row.names(data_to_melt)
    name.ATB.A <- processed_growth_data_list[[i]][1,"ATB.A"]
    name.ATB.A <- abbreviation_to_full_atb_name(name.ATB.A)
    name.ATB.B <- processed_growth_data_list[[i]][1,"ATB.B"]
    name.ATB.B <- abbreviation_to_full_atb_name(name.ATB.B)
    max.conc.A <- processed_growth_data_list[[i]][1,"Max.Conc.A"]
    max.conc.B <- processed_growth_data_list[[i]][1,"Max.Conc.B"]
    melted_data <-
      melt(subset(data_to_melt),
           id.vars = "ID")
    melted_data$ID <- as.factor(melted_data$ID)
    plot_OD <-
      ggplot(data = melted_data, aes(x = variable, y = ID)) +
      geom_tile(aes(fill = value), colour = "white") +
      scale_fill_gradient(low = "white",
                          high = "red",
                          name = "% growth") +
      scale_x_discrete(position = "top",
                       name=name.ATB.A,
                       labels=c(geomSeries(2,max.conc.A,9),0,"Ctl+","Ctl-"))+
      scale_y_discrete(position = "top",
                       name=name.ATB.B,
                       labels=c(geomSeries(2,max.conc.B,7),0),
                       limits=rev(levels(melted_data$ID)))+
      geom_text(aes(label = paste(round(value, 2)*100,"%"))) +
      theme(axis.text.x=element_text(angle=45,hjust=0))+
      ggtitle(names(processed_growth_data_list)[i])
    
    plot_list[[i]] <- plot_OD
    names(plot_list)[i] <- paste0(names(processed_growth_data_list)[i], "_plot_OD")
  }
  return(plot_list)
}