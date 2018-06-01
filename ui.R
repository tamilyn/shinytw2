shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML(paste("Shiny T", tags$sub("w"), tags$sup(2), sep = ""))),
  
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
              fluidRow(column(4, tableOutput("observationCounts")),
                       column(4, tableOutput("statisticalTestsOutput")),
                       column(4, tableOutput("results")))
   ))))
