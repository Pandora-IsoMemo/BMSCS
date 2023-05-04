modelVariablesImpTab <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Variable Importance",
    value = "modelVariablesImpTab",
    radioButtons(ns("impType"), label = "Variable importance type", choices = c("global", "model based (standardized coefficients)")),
    conditionalPanel(
      condition = "input.impType == 'model based (standardized coefficients)'",
      ns = ns,
    selectInput(ns("modelSelection"), "Select model", choices = "")
    ),
    tableOutput(ns("variableImp")),
    dataExportButton(ns("exportData"))
  )
}

modelVariablesImp <- function(input, output, session, model, modelAVG) {
  
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
    req(modelAVG())
    updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
  })
  
  dataFun <- reactive({
    req(model())
    
    function() {
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
      # for csv / excel - export:
      if(input$impType == "global"){
        variableImportance <- model()$variableData[, 1:2]
        names(variableImportance) <- c("Variable", "Importance")
        return(variableImportance)
      } else {
        importance <- abs(colMeans(extract(mPar)$betaAll))
        names(importance) <- mPar@varNames
        if(mPar@hasIntercept){
          importance <- importance[-1]
        }
        importance <- data.frame(Importance = importance[order(importance, decreasing  = TRUE)])
        variableImportance <- data.frame(Variable = rownames(importance), importance)
        return(variableImportance)
      }
    }
  })
  
  output$variableImp <- renderTable(dataFun()(), bordered = TRUE,
                                 rownames = FALSE, colnames = TRUE)

  callModule(dataExport, "exportData", data = dataFun, filename = "evaluation")
  
}