modelDWTab <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Durbin-Watson Test",
    value = "DW",
    selectInput(ns("t"), "Time variable (optional)", choices = NULL, multiple = FALSE, selected = NULL),
    selectInput(ns("modelSelection"), "Select Model", choices = ""),
    verbatimTextOutput(ns("DWsummary")),
    textExportButton(ns("exportText")),
    sliderInput(ns("lagDW"), "Maximum lag", min = 1, max = 12, step = 1, value = 1)
  )
}

modelDW <- function(input, output, session, model, data, modelAVG) {
  observe({
    updateSelectInput(session, "t", choices = c("", names(data())), selected = "")
  })

  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
    req(modelAVG())
    updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
  })

  allDW <- reactiveVal()
  observe({
    req(model())
    
    thisDW <- extractAllDW(allModels = model()$models,
                           maxLag = input$lagDW,
                           dependent = model()$dependent,
                           inDat = data(), 
                           asDataFrame = TRUE) %>% 
      bindAllResults(addEmptyRow = TRUE)
    
    allDW(thisDW)
  })
  
  tVar <- reactive({
    if (!is.null(input$t) & input$t != "") {
      return(data()[, input$t])
    } else {
      return(I(1:length(data()[, model()$dependent])))
    }
  })

  printFun <- reactive({
    req(model())
    req((input$modelSelection %in% names(model()$models)) || (input$modelSelection %in% names(modelAVG())))
    
    function() {
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
      printDWTest(inDat = data(),
                  dependent = model()$dependent,
                  mPar = mPar,
                  tVar = tVar(),
                  maxLag = input$lagDW)
    }
  })

  output$DWsummary <- renderPrint({
    req(model())

    printFun()()
  })

  callModule(textExport, "exportText", printFun = printFun, filename = "summary")
  
  return(allDW)
}

extractAllDW <- function(allModels, maxLag, dependent, inDat, asDataFrame = TRUE) {
  modelNames <- names(allModels)
  names(modelNames) <- modelNames
  
  lapply(modelNames, function(x) {
    res <- capture.output({
      printDWTest(inDat = inDat, 
                  dependent = dependent,
                  mPar = allModels[[x]], 
                  tVar = I(1:length(inDat[, dependent])), 
                  maxLag = maxLag)
    })
    
    if (asDataFrame) {
      res <- res %>%
        as.data.frame()
      colnames(res) <- "Durbin-Watson Test"
      
      res <- res %>%
        prefixNameAsColumn(name = "Model", value = x)
    }
    
    res
  })
}

printDWTest <- function(inDat, dependent, mPar, tVar, maxLag) {
  print(
    durbinWatsonTest(lm(I(inDat[, dependent] -
                            BMSC::predict(mPar, newdata = inDat)) ~
                          tVar), 
                     max.lag = maxLag)
  )
}
