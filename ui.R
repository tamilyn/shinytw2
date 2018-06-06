dashboardPage(skin = "purple",
  
  # Application title
  dashboardHeader( title = HTML(paste("Shiny T", tags$sub("w"), tags$sup(2), sep = ""))),
  
  dashboardSidebar(
    sidebarMenu(
       fileInput("rawCountFile", "Raw Count File"),
       fileInput("sampleDataFile", "Sample Data File"),

       selectInput("distanceMethod", "Distance", distance_choices, selected = "jsd"),
       uiOutput("strataFactor"),
       uiOutput("mainFactor") , 
       actionButton("addTestBtn", "Add Test")
    )),
    
    dashboardBody(
       fluidRow(plotOutput("plot")),
       fluidRow(box(DTOutput("testTable")),
                box(title = "Run Tests", status = "primary", 
                    numericInput("numPermutations", "Number of Permutations",
                    value = 999, 
                    min = 1, 
                    max = 100000),
                    actionButton("runTestsBtn", "Run Tests")
                    )),
       fluidRow(column(12, DTOutput("resultsTable"))) 
   ))
