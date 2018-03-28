## The server script
library(shiny)
library(fbutils)
  

# Define the server logic required to draw a heatmap
shinyServer( function(input, output, session) {
  
  ## Reactive expression for loading in data
  getData <- reactive({
    
    # Only load if the filepath is not NULL
    if (is.null(input$fbfile)) return(NULL)
    
    # Load data
    read.csv(input$fbfile$datapath, header = TRUE, as.is = TRUE)
  })

  # Return a boolean to determine if data was uploaded
  ## i.e. return FALSE if no data, TRUE if data
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  
  # Set options for this last output
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  
  
  ## The output of the function that creates a heatmap will be sent
  # to renderPlot such that the expression is "reactive" (updates with 
  # changing input) and the output type is a plot.
  
  output$heatmap <- renderPlot({
    
    # Return NULL if no data
    if (is.null(input$fbfile)) return(NULL)
    
    # Draw the heatmap
    fb.heatmap(fb.data.frame = getData(), plot.trait = input$traitsIn)
    
  })
  
  ## This code creates a selection input only if data has been uploaded already
  
  output$selectTrait <- renderUI({
  
    # Only load if the filepath is not NULL
    if (is.null(input$fbfile)) return(NULL)
    
    # Extract the numeric trait names
    traits <- colnames(getData()[-c(1:9)])
    # Remove the traits that are not numeric
    traits <- traits[which(sapply(X = getData()[,-c(1:9)], FUN = is.numeric))]
    
    selectInput("traitsIn", "Select a Trait to Heatmap", choices = traits)
  })


  
})