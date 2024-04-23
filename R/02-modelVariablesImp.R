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
    tags$br(),
    DTOutput(ns("variableImp")),
    tags$br(),
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
  
  allVariableImportance <- reactiveVal()
  observe({
    req(model())
    thisVarImp <- extractAllVariableImportance(models = model()$models,
                                               variableData = model()$variableData) %>%
      bindAllResults(addEmptyRow = TRUE) %>%
      dplyr::select("Model", "Variable", "Importance", "Estimate", "Sign")
    
    allVariableImportance(thisVarImp)
  })
  
  dataFun <- reactive({
    req(model())
    
    function() {
      if (length(model()) == 0) return(NULL)
      
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models
      } else {
        mPar <- modelAVG()
      }
      
      # for csv / excel - export:
      if(input$impType == "global"){
        extractAllVariableImportance(models = mPar, variableData = model()$variableData) %>%
          bind_rows() %>%
          dplyr::select("Model", "Variable", "Importance", "Estimate", "Sign")
      } else {
        extractModelBasedVariableImportance(model = mPar[[input$modelSelection]]) %>%
          joinRegressionSign(signData = extractSummary(model = mPar[[input$modelSelection]]) %>%
                               extractCoefTable() %>%
                               extractRegressionSign())
      }
    }
  })
  
  output$variableImp <- renderDT(dataFun()(), rownames = FALSE)

  shinyTools::dataExportServer("exportData", dataFun = dataFun, filename = "variableImportance")
  
  return(allVariableImportance)
}

# extract importance ----

extractGlobalVariableImportance <- function(variableData) {
  variableImportance <- variableData[, 1:2]
  names(variableImportance) <- c("Variable", "Importance")
  variableImportance$Importance <- variableImportance$Importance %>% 
    round(digits = 2)
  return(variableImportance)
}

extractModelBasedVariableImportance <- function(model) {
  importance <- abs(colMeans(extract(model)$betaAll))
  names(importance) <- model@varNames
  if(model@hasIntercept){
    importance <- importance[-1] %>% 
      round(digits = 2)
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

extractAllVariableImportance <- function(models, variableData) {
  globalImportance <- extractGlobalVariableImportance(variableData = variableData)
  globalImportance$Model <- "global"
  globalImportance$Estimate <- NA
  globalImportance$Sign <- NA
  
  modelNames <- names(models)
  names(modelNames) <- modelNames
  modelBasedImportance <- lapply(modelNames, function(name) {
    extractModelBasedVariableImportance(model = models[[name]]) %>%
      joinRegressionSign(signData = extractSummary(model = models[[name]]) %>%
                           extractCoefTable() %>%
                           extractRegressionSign()) %>%
      prefixNameAsColumn(name = "Model", value = name)
  })
  
  c(list(global = globalImportance), modelBasedImportance)
}

joinRegressionSign <- function(importance, signData) {
  importance %>%
    dplyr::left_join(signData, by = "Variable")
}
