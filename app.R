setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(data.table)
library(dplyr)
library(grid)
source("utilities.R")
source("plot_function.R")

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
