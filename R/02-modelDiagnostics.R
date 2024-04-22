modelDiagnosticsTab <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Model Diagnostics",
    value = "summaryTab",
    selectInput(ns("modelSelection"), "Select model", choices = ""),
    radioButtons(ns("diagType"), label = "Diagnostics Type", choices = getDiagnosticTypes()),
    verbatimTextOutput(ns("diagnostics")),
    textExportButton(ns("exportText"))
  )
}

getDiagnosticTypes <- function() {
  c("Gelman Scale Reduction Factor" = "gelman",
    "Raftery and Lewis" = "raftery",
    "Geweke z-Score" = "geweke",
    "Heidelberger-Welch" = "heidel")
}

modelDiagnostics <- function(input, output, session, model, nChains) {
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
  })
  
  allDiagnostics <- reactiveVal()
  observe({
    req(model())
    
    thisDiagnostics <- extractAllDiagnostics(allModels = model()$models,
                                             nChains = nChains) %>% 
      bindAllResults(addEmptyRow = TRUE)
    
    allDiagnostics(thisDiagnostics)
  })
  
  printFun <- reactive({
    req(model())
    req((input$modelSelection %in% names(model()$models)))
    
    function() {
      printDiagnostics(allModels = model()$models,
                       modelName = input$modelSelection,
                       nChains = nChains,
                       diagType = input$diagType)
    }
  })
  
  output$diagnostics <- renderPrint({
    req(model())
    printFun()()
  })
  
  callModule(textExport, "exportText", printFun = printFun, filename = "diagnostics")
  
  return(allDiagnostics)
}

extractAllDiagnostics <- function(allModels, nChains, asDataFrame = TRUE) {
  modelNames <- names(allModels)
  names(modelNames) <- modelNames
  
  lapply(modelNames, function(x) {
    # one model per resRow
    allDiagTypes <- names(getDiagnosticTypes())
    names(allDiagTypes) <- names(getDiagnosticTypes())
    resRow <- lapply(allDiagTypes, function(type) {
      # one diagnostic per resCol
      resCol <- capture.output({
        printDiagnostics(allModels = allModels,
                         modelName = x,
                         nChains = nChains,
                         diagType = getDiagnosticTypes()[type])
      })
      if (asDataFrame) {
        # remove first row if empty
        if (gsub(" ", "", resCol[1]) == "") resCol <- resCol[2:length(resCol)]
        
        # convert to dataframe
        resCol <- resCol %>%
          as.data.frame()
        colnames(resCol) <- type
      }
      
      resCol
    })
    
    if (asDataFrame) {
      # extend shorter columns to length of longer columns
      maxNRows <- max(sapply(resRow, nrow))
      
      resRow <- lapply(names(resRow), function(type) {
        resCol <- resRow[[type]]
        if (nrow(resCol) < maxNRows) {
          tmp <- resCol[[type]]
          length(tmp) <- maxNRows
          tmp <- tmp %>%
            as.data.frame()
          colnames(tmp) <- type
          
          resCol <- tmp
        }
        
        resCol
      })
      
      # bind columns of diagnostics
      resRow <- resRow %>%
        bind_cols() %>%
        prefixNameAsColumn(name = "model", value = x)
    }
    
    resRow
  })
}

printDiagnostics <- function(allModels, modelName, nChains, diagType) {
  varNames <- allModels[[modelName]]@varNames
  parameters <- extract(allModels[[modelName]])$betaAll
  colnames(parameters) <- varNames
  parameters <- as.data.frame(parameters)
  diag <- convergenceDiagnostics(parameters, nChains)
  print(diag[[diagType]])
}

convergenceDiagnostics <- function(parameters, nChains){
  splitChains <- factor(rep(1:nChains, each = nrow(parameters) / nChains))
  
  mcmcObject <- split(parameters, splitChains)
  
  mcmcObject <- lapply(mcmcObject, function(x){
    x <- as.matrix(x)
    x <- mcmc(x, start = 1, end = nrow(x))
    x
  })
  
  raftery <- try({raftery.diag(parameters)}, silent = TRUE)
  gelman <- try({gelman.diag(mcmcObject, autoburnin = FALSE, multivariate = FALSE)}, silent = TRUE)
  geweke <- try({geweke.diag(mcmcObject)}, silent = TRUE)
  heidel <- try({heidel.diag(parameters)}, silent = TRUE)
  
  if(nChains == 1){
    gelman <- "For Gelman-Rubin diagnostics, at least 2 chains are required.
    Number of chains option available in the model options tab"
  }
  
  return(list(raftery = raftery, gelman = gelman, geweke = geweke, heidel = heidel))
}