#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)
library(grid)

false_buzzer_cleaner <- function(x){
  subject_data_cleaned <- x
  if(0 %in% subject_data_cleaned$taskNumber){
    subject_data_cleaned <- subject_data_cleaned[-which(subject_data_cleaned$taskNumber == 0),]
  }
  for (i in 1:length(unique(subject_data_cleaned$taskNumber))){
    false_buzzer_hits <- (subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)][1] == subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)][length(subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)])])
    
    if (false_buzzer_hits == T) {
      subject_data_cleaned <- subject_data_cleaned[-(which(subject_data_cleaned$taskNumber == i)),]
    }
  }
  return(subject_data_cleaned)
}

minutes_seconds <- function (x){
  x <- gsub("M ", ":", x)
  x <- gsub("S", "", x)
  return(x)
}

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
    scale_y_continuous(name = "Distance in meters (feet)", 
                       limits=c(0, 11), 
                       breaks=seq(0, 10, by = 2.5),
                       labels=c("0 m\n(0 ft.)", "2.5 m\n(8.2 ft.)", "5 m\n(16.4 ft.)", "7.5 m\n(24.6 ft.)", "10 m\n(32.8 ft.)"),
                       sec.axis = sec_axis(~.*25, name = 'Rotation Inaccuracy in degrees', breaks = seq(0, 300, by = 50))) +
    scale_x_continuous(breaks = round(seq(from = min(subject_data$elapsedTime), to = max(subject_data$elapsedTime), length.out = 10)),
                       labels = minutes_seconds(seconds_to_period(round(seq(from = min(subject_data$elapsedTime), to = max(subject_data$elapsedTime), length.out = 10))))) +
    coord_cartesian(clip='off') +
    labs(title = "Accuracy Statistics ", 
         x = "Time (Minutes:Seconds) ",
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
                 linetype="dotdash", 
                 size=0.7)
  }
  return(whole_plot)
}

options(shiny.maxRequestSize = 200*1024^2)
options(digits=5)

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Please Review the RUI usage performance"),
  
  fluidRow(
    
    column(12,
           plotOutput("performance_plot"),
           div(style = "height:300px"))
    
  ),
  
  fluidRow(
    wellPanel(fileInput("file1", "Choose CSV File", accept = ".csv"))
  )
  
  # Sidebar with a input 
  # sidebarLayout(
  #   sidebarPanel(
  # 
  #     fileInput("file1", "Choose CSV File", accept = ".csv")
  #     
  #   ),
  #   # Show a plot of the generated distribution
  #   mainPanel(
  #     plotOutput("performance_plot")
  #   )
  # )
)


# Define server logic
server <- function(input, output) {
  
  #req(input$file1)
  #subject_data <- reactive({fread(file1$datapath)})
  
  output$performance_plot <- renderPlot({
    
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    plot_complexity_data(fread(file$datapath))
  }, height = 600)
}
# Run the application 
shinyApp(ui = ui, server = server)

