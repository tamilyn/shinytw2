
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
      selectInput('mainFactor', 'Select main factor', af )
    }
  })
  
  output$strataFactor = renderUI({
    af <- availableFactors()
    if(length(af) > 0) {
      selectInput('strataFactor', 'Select strata factor', af )
    }
  })

  physeqData <- reactive({
     req(sampleData(), rawCountData())

     sdata <- sampleData()
     asv.t <- rawCountData()

     phyloseq( otu_table(asv.t, taxa_are_rows = F), sample_data(sdata))
  })

  physeqDataFactor <- reactive({

     physeq <- physeqData()
     message(str_c("physeqDataFactor: ", input$mainFactor))
     sample_data(physeq)[[input$mainFactor]] 

     physeq
  })

  distanceMatrices <- reactive({
    req(physeqData())
    
    physeq <- physeqDataFactor()
    dist_matrices = distance(physeq, method=c(input$distanceMethod))
  })
  
  output$plot <- renderPlot({
     req(physeqData(), input$mainFactor)

     physeq <- physeqDataFactor()
     dist_matrices = distance(physeq, method=c(input$distanceMethod))

     ordination_list = ordinate(physeq, method="MDS",
            distance = distanceMatrices())

     p <- plot_ordination(physeq = physeq,
         ordination = ordination_list,
         type = "samples",
         color = input$mainFactor ) +
         theme_minimal()

     return(p)
  })

  statisticalTestOutput <- reactive({
    physeq <- physeqData()
    sample_data(physeq)[[input$mainFactor]] 
    dist_matrices = distance(physeq, method=c(input$distanceMethod))
    
    tres = WT.test(dist_matrices, 
                   sample_data(physeq)[[input$mainFactor]],
                   sample_data(physeq)[[input$strataFactor]],
                   nrep = input$numPermutations)

    message(str_c("Got response from WT.test"))
    message(class(tres))
    if(is.list(tres)) {
        message(str_c("List WT.test ", str_c(names(tres), collapse=", ")))
    }
    tres
  })
  
  output$statisticalTestsOutput <- renderText({
    req(statisticalOutput(), input$runPermutations)
  
    isolate(statisticalOutput())
  })
  
  statisticalOutput <- eventReactive(input$runPermutations, {
    print("statistical output run permutations:")
    st <- statisticalTestOutput()
    if(is.list(st)) {
      vals <- map2_chr(names(st), st,  ~ str_c( .x, " = ", .y ))
      str_c(vals, collapse = ", ")
    } else {
      str_c( st )
    }
  })
})
