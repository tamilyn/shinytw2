shinyUI(fluidPage(
  
  # Application title
  titlePanel("Shiny Tw2"),
  
  sidebarLayout(
    sidebarPanel(
       fileInput("rawCountFile", "Raw Count File"),
       fileInput("sampleDataFile", "Sample Data File"),
       uiOutput("mainFactor"), 
       selectInput("distanceMethod", "Distance", distance_choices, selected = "jsd")
    ),
    
    mainPanel(plotOutput("plot"))
)))
