
shinyServer(function(input, output) {

  rawCountData <- reactive({
    infile = input$rawCountFile
    if(is.null(infile)) return(NULL)

    asv = read.table(infile$datapath, row.names=1, sep = ",", check.names=F, header = T)
    return(t(asv))
  })

  sampleData <- reactive({
    infile = input$sampleDataFile
    if(is.null(infile)) return(NULL)

    sdata = read.csv(infile$datapath)
    rownames(sdata) <- as.character(sdata$id_full)
    return(sdata)
  })

  availableFactors <- reactive({
      sd <- sampleData()
      if(!is.null(sd)) {
        choices <- colnames(sd)
        # ignore first column, which is X
        choices[2:length(choices)]
      }
  })

  output$mainFactor = renderUI({
    af <- availableFactors()
    if(length(af) > 0) {
      selectInput('selectedFactor', 'Select factor', af )
    } else {
      p("No available factors")
    }
  })

  physeqData <- reactive({
     req(sampleData(), rawCountData())

     sdata <- sampleData()
     asv.t <- rawCountData()

     phyloseq( otu_table(asv.t, taxa_are_rows = F), sample_data(sdata))
  })

  output$plot <- renderPlot({
     req(physeqData())

     physeq <- physeqData()

     sample_data(physeq)[[input$selectedFactor]] 
     dist_matrices = distance(physeq, method=c(input$distanceMethod))

     ordination_list = ordinate(physeq, method="MDS",
            distance = dist_matrices)

     p <- plot_ordination(physeq = physeq,
         ordination = ordination_list,
         type = "samples",
         color = input$selectedFactor ) +
         theme_minimal()

     return(p)
  })

})
