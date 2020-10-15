plot_complexity_data <- function(x){

  subject_data <- x
  subject_data <- subject_data[which(subject_data$ExperimentState == "Complexity"),]
  subject_data <- subject_data %>%
    false_buzzer_cleaner()
  
  colors <- c("Distance to target" = "#CC6666", "Rotation Inaccuracy" = "#66CC99")
              
  centroidPlot <- ggplot(data=subject_data) +
    geom_line(aes(x = elapsedTime, y = distance, group=taskNumber, color = "Distance to target"), size = 1) 
  
  whole_plot <- centroidPlot +
    geom_line(aes(x = elapsedTime, y = angle/25, group=taskNumber, color = "Rotation Inaccuracy"), size = 1) +
    scale_y_continuous(name = "Distance", sec.axis = sec_axis(~.*25, name = 'Rotation Inaccuracy (Degrees)')) +
    scale_x_continuous(breaks = round(seq(min(subject_data$elapsedTime), by = 100, length.out = max(subject_data$elapsedTime)))) +
    coord_cartesian(clip='off') +
    labs(title = "Accuracy Statistics ", 
         x = "Time (seconds) ",
         color = "Legend: ") +
    
    theme(axis.title.x = element_text(margin=margin(54,0,0,0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          legend.position = "bottom",
          legend.text = element_text(size=15),
          legend.title = element_text(size=15, face="bold"))
  
  for (i in 1:length(unique(subject_data$taskNumber))) {
    whole_plot <- whole_plot + annotation_custom(
    textGrob(
        label=paste0('Task ',unique(subject_data$taskNumber)[i]), rot=90, gp=gpar(fontsize=12)),
      xmin = subject_data$elapsedTime[which(subject_data$taskNumber == unique(subject_data$taskNumber)[i])][1], 
      xmax = subject_data$elapsedTime[which(subject_data$taskNumber == unique(subject_data$taskNumber)[i])][length(subject_data$elapsedTime[which(subject_data$taskNumber == unique(subject_data$taskNumber)[i])])],
    #ymin = -((max(subject_data$distance)/4) +1), ymax = -(max(subject_data$distance)/4)) +
    ymin = -1.3, ymax = -2.3) +
      geom_vline(xintercept = subject_data$elapsedTime[which(subject_data$taskNumber == unique(subject_data$taskNumber)[i])][length(subject_data$elapsedTime[which(subject_data$taskNumber == unique(subject_data$taskNumber)[i])])],
                 linetype="dotted", 
                 size=0.5)
  }
  return(whole_plot)
}

