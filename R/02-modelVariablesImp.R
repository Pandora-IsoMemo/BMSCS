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

modelVariablesImp <- function(input, output, session, model) {
  
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
  })
  
  dataFun <- reactive({
    req(model())
    
    function() {
      # for csv / excel - export:
      if(input$impType == "global"){
        variableImportance <- model()$variableData[, 1:2]
        names(variableImportance) <- c("Variable", "Importance")
        return(variableImportance)
      } else {
        importance <- abs(colMeans(extract(model()$models[[input$modelSelection]])$betaAll))
        names(importance) <- model()$models[[input$modelSelection]]@varNames
        if(model()$models[[input$modelSelection]]@hasIntercept){
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