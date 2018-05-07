
shinyServer(function(input, output) {

  values <- reactiveValues(sampleMainFactor = NULL)

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
     req(input$mainFactor)

     physeq <- physeqData()
     message(str_c("physeqDataFactor: ", input$mainFactor))

     # save main factor 
     values$sampleMainFactor = sample_data(physeq)[[input$mainFactor]]

     physeq
  })

  distanceMatrices <- reactive({
    req(physeqDataFactor())
    
    physeq <- physeqDataFactor()
    dist_matrices = distance(physeq, method=c(input$distanceMethod))
  })
  
  output$plot <- renderPlot({
     req(physeqDataFactor(), input$mainFactor)

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

  observationCounts <- reactive({
    physeq <- physeqDataFactor()

    #zz <- table(sample_data(physeq)[[input$mainFactor]], useNA="ifany")

    zz <- table(values$sampleMainFactor, useNA="ifany")
    df <- data_frame(value=names(zz), count=zz)
    df
  })

  output$observationCounts  <- renderTable({
    observationCounts()
  })

  computeTres <- reactive({

    physeq <- physeqDataFactor()
    dist_matrices = distance(physeq, method=c(input$distanceMethod))
    
    tres = WT.test(dist_matrices, 
                   values$sampleMainFactor, 
                   sample_data(physeq)[[input$strataFactor]],
                   nrep = input$numPermutations)

    if(!is.list(tres)) {
      message(str_c("tres results invalid", class(tres)))
      message(tres)
      return(NULL)
    }
    tres
  })
  
  distTestOutput <- reactive({

    tres = computeTres()

    physeq <- physeqDataFactor()
    dist_matrices = distance(physeq, method=c(input$distanceMethod))

    tres$d = dist.cohen.d(dist_matrices, values$sampleMainFactor)
    df <- data_frame(name=names(tres$d), value=tres$d)
    df
  })

  statisticalTestOutput <- reactive({
      tres = computeTres()
  })
  
  output$statisticalTestsOutput <- renderTable({
    req(statisticalOutput(), input$runPermutations)
  
    isolate(statisticalOutput())
  })


  output$results <- renderTable({
    req(statisticalOutput(), input$runPermutations)
  
    isolate(distTestOutput())
  })
  
  statisticalOutput <- eventReactive(input$runPermutations, {
    st <- statisticalTestOutput()
    if(is.list(st)) {
      df <- data_frame(name=names(st), value=st)
    }
  })
})
