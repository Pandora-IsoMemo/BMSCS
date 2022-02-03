modelDiagnosticsTab <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Model Diagnostics",
    value = "summaryTab",
    selectInput(ns("modelSelection"), "Select model", choices = ""),
    radioButtons(ns("diagType"), label = "Diagnostics Type", choices = c("Gelman Scale Reduction Factor" = "gelman",
                                                                         "Raftery and Lewis" = "raftery",
                                                                         "Geweke z-Score" = "geweke",
                                                                         "Heidelberger-Welch" = "heidel")),
    verbatimTextOutput(ns("diagnostics")),
    textExportButton(ns("exportText"))
  )
}

modelDiagnostics <- function(input, output, session, model, nChains) {
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
  })
  
  printFun <- reactive({
    req(model())
    req((input$modelSelection %in% names(model()$models)))
    
    function() {
      varNames <- model()$models[[input$modelSelection]]@varNames
      parameters <- extract(model()$models[[input$modelSelection]])$betaAll
      colnames(parameters) <- varNames
      parameters <- as.data.frame(parameters)
      diag <- convergenceDiagnostics(parameters, nChains)
      diagType <- input$diagType
      print(diag[[diagType]])
    }
  })
  
  output$diagnostics <- renderPrint({
    req(model())
    printFun()()
  })
  
  callModule(textExport, "exportText", printFun = printFun, filename = "diagnostics")
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