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
        mPar <- model()$models
      } else {
        mPar <- modelAVG()
      }
      
      # for csv / excel - export:
      extractVariableImportance(model = mPar[[input$modelSelection]], 
                                variableData = model()$variableData,
                                importanceType = input$impType)
    }
  })
  
  output$variableImp <- renderTable(dataFun()(), bordered = TRUE,
                                 rownames = FALSE, colnames = TRUE)

  callModule(dataExport, "exportData", data = dataFun, filename = "evaluation")
  
}

# extract importance ----

extractAllVariableImportance <- function(models) {
  globalImportance <- extractGlobalVariableImportance(variableData = models$variableData)
  globalImportance$Model <- "global"
  globalImportance$Estimate <- NA
  globalImportance$Sign <- NA
  
  modelBasedImportance <- lapply(models$models, function(model) {
    extractModelBasedVariableImportance(model = model) %>%
      joinRegressionSign(signData = extractSummary(model = model) %>%
                           extractCoefTable() %>%
                           extractRegressionSign())
  }) %>%
    bind_rows(.id = "Model")
  
  list(globalImportance, modelBasedImportance) %>%
    bind_rows() %>%
    dplyr::select("Model", "Variable", "Importance", "Estimate", "Sign")
}

extractVariableImportance <- function(model, variableData, importanceType) {
  if(importanceType == "global"){
    extractGlobalVariableImportance(variableData = variableData)
  } else {
    extractModelBasedVariableImportance(model = model) %>%
      joinRegressionSign(signData = extractSummary(model = model) %>%
                           extractCoefTable() %>%
                           extractRegressionSign())
  }
}

extractGlobalVariableImportance <- function(variableData) {
  variableImportance <- variableData[, 1:2]
  names(variableImportance) <- c("Variable", "Importance")
  return(variableImportance)
}

extractModelBasedVariableImportance <- function(model) {
  importance <- abs(colMeans(extract(model)$betaAll))
  names(importance) <- model@varNames
  if(model@hasIntercept){
    importance <- importance[-1]
  }
  importance <- data.frame(Importance = importance[order(importance, decreasing  = TRUE)])
  variableImportance <- data.frame(Variable = rownames(importance), importance)
  return(variableImportance)
}

# extract sign ----

extractCoefTable <- function(capturedOutput) {
  summary_rows <- strsplit(capturedOutput, "\\s+")
  
  coefTable <- summary_rows[c(3:(length(capturedOutput)-4))]
  # create and update column names
  coefTable[[1]][coefTable[[1]] == "Cred_Interval_"] <- "Cred_Interval_Min"
  coefTable[[1]] <- c(coefTable[[1]], "Cred_Interval_Max")
  
  coefTable_columns <- t(sapply(coefTable, function(row) row)) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(.[1, ])
  coefTable_columns <- coefTable_columns[2:nrow(coefTable_columns),] 
  
  # clean up columns
  coefTable_columns$Estimate <- as.numeric(coefTable_columns$Estimate)
  coefTable_columns$Median <- as.numeric(coefTable_columns$Median)
  coefTable_columns$SD <- as.numeric(coefTable_columns$SD)
  coefTable_columns$Cred_Interval_Min <- coefTable_columns$Cred_Interval_Min %>%
    gsub(pattern = "\\[|\\]|\\,", replacement = "")
  coefTable_columns$Cred_Interval_Max <- coefTable_columns$Cred_Interval_Max %>%
    gsub(pattern = "\\[|\\]|\\,", replacement = "")
  
  coefTable_columns
}

extractRegressionSign <- function(coefTable) {
  estimate <- coefTable[2:nrow(coefTable), 1:2]
  colnames(estimate)[1] <- "Variable"
  estimate$Sign <- getSign(estimate$Estimate)
  
  estimate
}

getSign <- function(values) {
  -1*(values < 0) + 1*(values > 0)
}

# combine importance and sign ----

joinRegressionSign <- function(importance, signData) {
  importance %>%
    dplyr::left_join(signData, by = "Variable")
}

