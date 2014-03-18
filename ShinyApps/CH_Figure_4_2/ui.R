library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("AIC vs. BIC - Simple Normal Example"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    
    sliderInput("n", 'Sample Size', 
                min = 100, max = 5000, value = 100, step= 100)
    
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    h3(textOutput("caption")),
    
    plotOutput("RiskPlot")
  )
))