shinyServer(function(input, output) {

  values <- reactiveValues(sampleMainFactor = NULL, 
                           testsDT = data_frame(),
                           resultsDT = NULL)

  # rawCountData ----
  rawCountData <- reactive({
    infile = input$rawCountFile
    if(is.null(infile)) return(NULL)

    asv = read.table(infile$datapath, row.names = 1, sep = ",", check.names = FALSE, header = TRUE)
    return(t(asv))
  })

  # sampleData ----
  sampleData <- reactive({
    infile = input$sampleDataFile
    if(is.null(infile)) return(NULL)

    sdata = read.csv(infile$datapath)
    rownames(sdata) <- as.character(sdata$id_full)
    return(sdata)
  })

  # availableFactors ----
  availableFactors <- reactive({
      sd <- sampleData()
      if(!is.null(sd)) {
        choices <- colnames(sd)
        # ignore first column, which is X
        choices[2:length(choices)]
      }
  })

  # output$mainFactor ----
  output$mainFactor = renderUI({
    af <- availableFactors()
    if(length(af) > 0) {
      selectInput('mainFactor', 'Select main factor', af )
    }
  })
  
  # output$strataFactor ----
  output$strataFactor = renderUI({
    af <- append("None", availableFactors())
    if(length(af) > 0) {
      selectInput('strataFactor', 'Select strata factor', af )
    }
  })

  # output$physeqData ----
  physeqData <- reactive({
     req(sampleData(), rawCountData())

     sdata <- sampleData()
     asv.t <- rawCountData()

     phyloseq( otu_table(asv.t, taxa_are_rows = F), sample_data(sdata))
  })


  # physeqDataFactor ----
  physeqDataFactor <- reactive({
     req(input$mainFactor)

     physeq <- physeqData()

     # save main factor 
     values$sampleMainFactor = sample_data(physeq)[[input$mainFactor]]

     physeq
  })

  # distanceMatrices ----
  distanceMatrices <- reactive({
    req(physeqDataFactor())
    
    physeq <- physeqDataFactor()
    dist_matrices = distance(physeq, method=c(input$distanceMethod))
  })
  
  # output$plot ----
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
     
     p <- p + scale_color_discrete(labels = paste(levels(values$sampleMainFactor), 
                                          table(values$sampleMainFactor))) 

     return(p)
  })

  # observationCounts ----
  observationCounts <- reactive({
    physeq <- physeqDataFactor()

    zz <- table(values$sampleMainFactor, useNA="ifany")
    as_data_frame(zz)
  })

  # output$observationCounts ----
  output$observationCounts  <- renderTable({
    observationCounts()
  })

  # computeTresFtn ----
  computeTresFtn <- function(strata, mainf, dist) {

    #print(glue::glue("strata {strata} main {mainf} dist {dist}"))

    sdata <- sampleData()
    asv.t <- rawCountData()

    phyloseq( otu_table(asv.t, taxa_are_rows = F), sample_data(sdata))

    physeq <- physeqData()
    dist_matrices = distance(physeq, method = dist)
    tres = WT.test(dist_matrices, 
                   sample_data(physeq)[[mainf]],
                   sample_data(physeq)[[strata]],
                   nrep = input$numPermutations)
  }

  # computeTres ----
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
  
  # distTestOutput ----
  distTestOutput <- reactive({

    tres = computeTres()
    dist_matrices = distance(physeqDataFactor(), method=c(input$distanceMethod))

    tres$d = dist.cohen.d(dist_matrices, values$sampleMainFactor)
    df <- as_data_frame(tres$d) %>% rename(dist_cohen_d = value)
  })

  
  # output$statisticalTestsOutput ----
  output$statisticalTestsOutput <- renderTable({
    req(input$mainFactor, input$strataFactor, statisticalOutput(), input$runPermutations)
  
    #print(str_c("computing statistical ", input$strataFactor, input$mainFactor, sep = " "))
    isolate(computeTres())
  }, digits = 5)

  
  # output$results ----
  output$results <- renderTable({
    req(statisticalOutput(), input$runPermutations)
  
    isolate(distTestOutput())
  }, digits = 5)
  
  # statisticalOutput ----
  statisticalOutput <- eventReactive(input$runPermutations, {
    st <- computeTres()
    if(is.list(st)) {
      df <- as_data_frame(st)
    }
  })

  sayecho <- function(tid, strata, mainf, dist) {
    status <- "SUCCESS"
    err <- NULL
    x <- tryCatch({
            computeTresFtn(strata, mainf, dist) 
    }, warning = function(w) {
            print(glue::glue("warning: {w}"))
            NULL
    }, error = function(e) {
            err = e
            #print(glue::glue("error : {err}"))
            NULL
    })

  if(!is.null(x)) {
    data_frame(p.value = x[["p.value"]], 
               nrep = x[["nrep"]],
               main_factor = mainf,
               test_id = tid,
               dist_cohen_d = x[["t.stat"]],
               status = "SUCCESS")
  } else {
    data_frame(p.value = 0, 
               nrep = 0, 
               main_factor = mainf,
               test_id = tid,
               dist_cohen_d = 0, 
               status = str_c("ERROR: ", err))
  }
  }

  # RUN TESTS BUTTON ----
  observeEvent(input$runTestsBtn, {
    w_df = map(1:nrow(values$testsDT), ~
                sayecho(values$testsDT$test_id[.],
                        values$testsDT$strata_factor[.],
                        values$testsDT$main_factor[.],
                        values$testsDT$distance[.]))  %>%
    bind_rows()
    values$resultsDT <-  w_df
  })

  # ADD TEST BUTTON ----
  observeEvent(input$addTestBtn, {
     req(input$mainFactor)
     #browser()
     aRow <- data_frame(test_id = input$addTestBtn,
                        strata_factor = input$strataFactor,
                        main_factor = input$mainFactor,
                        distance = input$distanceMethod)
     if(is.null(values$testsDT)) {
       values$testsDT = aRow
     } else {
       newTable <- bind_rows(values$testsDT, aRow)
       values$testsDT =  newTable
     }
  })

  testsData <- reactive({
     t <- values$testsDT 
     t
  })

  resultsData <- reactive({
    values$resultsDT
  })

  output$testTable <- renderDT({
    datatable(testsData(), caption = "Tests to Run")
  })
  output$resultsTable <- renderDT({
    datatable(resultsData(), caption = "Test Results")
  })
})
