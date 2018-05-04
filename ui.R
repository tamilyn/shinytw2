shinyUI(fluidPage(
  
  # Application title
  titlePanel("Shiny Tw2"),
  
  sidebarLayout(
    sidebarPanel(
       fileInput("rawCountFile", "Raw Count File"),
       fileInput("sampleDataFile", "Sample Data File"),
       selectInput("distanceMethod", "Distance", distance_choices, selected = "jsd"),
       uiOutput("strataFactor"),
       uiOutput("mainFactor"), 
       numericInput("numPermutations", "Number of Permutations",
                    value = 999, 
                    min = 1, 
                    max = 100000),
       actionButton("runPermutations", "Run Permuations")
    ),
    
    mainPanel(plotOutput("plot"),
              textOutput("statisticalTestsOutput")) #eeds to be isolated
)))
