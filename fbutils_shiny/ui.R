## User interface script
library(shiny)

# Define the UI for an application that creates a heatmap from a file
shinyUI(fluidPage(
  
  # Title
  titlePanel("Field Book Visualization"),
  
  # Sidebar to load a CSV data file
  sidebarLayout(
    sidebarPanel(
      
      # Help text
      helpText("Upload a CSV of data from a field book table file."),
      
      # Data file input
      fileInput(inputId = "fbfile",
                label = "Upload Field Book Table File",
                accept = ".csv" ),
      
      # Conditional buttons based on whether a file was uploaded
      conditionalPanel(
        condition = "output.fileUploaded",
        
        uiOutput("selectTrait")
        
      )
    ),
    
    mainPanel(
      plotOutput("heatmap")
    )
  )
  
))