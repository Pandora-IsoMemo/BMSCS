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
      bindAllResults(addEmptyRow = TRUE) %>%
      shinyTryCatch(errorTitle = "Extracting model results failed")
    
    allDiagnostics(thisDiagnostics)
  })
  
  printFun <- reactive({
    function() {
      if (length(model()) == 0 ||
          !((input$modelSelection %in% names(model()$models))))
        return(NULL)
      
      printDiagnostics(
        allModels = model()$models,
        modelName = input$modelSelection,
        nChains = nChains,
        diagType = input$diagType
      )
    }
  })
  
  output$diagnostics <- renderPrint({
    req(model())
    printFun()()
  })
  
  textExportServer("exportText", outFun = printFun, filename = "diagnostics")
  
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
        prefixNameAsColumn(name = "Model", value = x)
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
  #splitChains <- factor(rep(1:nChains, each = nrow(parameters) / nChains))
  
  # fix error in split() if we have different sizes of parameters and splitChains:
  stopifnot(is.data.frame(parameters) || is.matrix(parameters))
  
  n <- nrow(parameters)
  
  if (nChains > n) {
    warning("nChains > number of rows; reducing nChains to ", n)
    nChains <- n
  }
  
  # compute contiguous, near-equal block sizes that sum to n
  base_size <- n %/% nChains
  remainder <- n %% nChains
  sizes <- rep(base_size, nChains)
  if (remainder > 0) sizes[seq_len(remainder)] <- sizes[seq_len(remainder)] + 1
  
  # build a factor of length n with contiguous blocks per chain
  splitChains <- factor(rep(seq_len(nChains), times = sizes))
  
  mcmcObject <- split(parameters, splitChains)
  
  # to coda::mcmc (preserve order)
  mcmcObject <- lapply(mcmcObject, function(x){
    x <- as.matrix(x)
    #mcmc(x, start = 1, end = nrow(x))
    mcmc(x, start = 1, thin = 1)
  })
  
  # --- align start/end/thin across chains for mcmc.list() ---
  # (starts all 1; choose end = shortest, thin = 1)
  min_len <- min(vapply(mcmcObject, nrow, integer(1)))
  if (min_len < 2L) {
    warning("Chains too short after alignment for Gelman/Geweke.")
  }
  mcmcAligned <- lapply(mcmcObject, function(mc) window(mc, start = 1, end = min_len, thin = 1))
  mcmcList <- try(mcmc.list(mcmcAligned), silent = TRUE)
  
  # Single-chain diags can operate on the full matrix
  raftery <- try(raftery.diag(mcmc(as.matrix(parameters), start = 1, thin = 1)), silent = TRUE)
  heidel <- try(heidel.diag(mcmc(as.matrix(parameters), start = 1, thin = 1)), silent = TRUE)
  
  gelman <- try(gelman.diag(mcmcList, autoburnin = FALSE, multivariate = FALSE), silent = TRUE)
  geweke <- try(geweke.diag(mcmcList), silent = TRUE)
  
  if(nChains == 1){
    gelman <- "For Gelman-Rubin diagnostics, at least 2 chains are required.
    Number of chains option available in the model options tab"
  }
  
  return(list(raftery = raftery, gelman = gelman, geweke = geweke, heidel = heidel))
}