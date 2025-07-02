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
    dataExportButton(ns("exportVariableImportanceData"))
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
    function() {
      if (length(model()) == 0) return(NULL)
      
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models
      } else {
        if (length(modelAVG()) == 0) return(NULL)
        mPar <- modelAVG()
      }
      
      # for csv / excel - export:
      if(input$impType == "global"){
        extractAllVariableImportance(models = mPar, variableData = model()$variableData) %>%
          bind_rows() %>%
          dplyr::select("Model", "Variable", "Importance", "Estimate", "Sign")
      } else {
        extractModelBasedVariableImportance(model = mPar[[input$modelSelection]]) %>%
          joinRegressionSign(signData = mPar[[input$modelSelection]] %>%
                               extract_coeff_from_model() %>%
                               extractRegressionSign())
      }
    }
  })
  
  output$variableImp <- renderDT(dataFun()(), rownames = FALSE)

  shinyTools::dataExportServer("exportVariableImportanceData", dataFun = dataFun, filename = "variableImportance")
  
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
    as.data.frame(stringsAsFactors = FALSE)
  
  coefTable_columns <- coefTable_columns %>%
    setNames(coefTable_columns[1, ])
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

extract_coeff_from_model <- function(model, cLevel = 0.95) {
  stopifnot(inherits(model, "ConstrainedLinReg"))
  
  # Credible interval boundaries
  lower <- (1 - cLevel) / 2
  upper <- 1 - lower
  probs <- c(lower, 0.5, upper)
  
  # Read draws
  draws <- rstan::extract(model, pars = "betaAll", permuted = TRUE)$betaAll
  var_names <- model@varNames
  n_beta <- ncol(draws)
  n_varnames <- length(var_names)
  
  # Check if "(Intercept)" is in varNames
  intercept_in_varnames <- any(grepl("Intercept", var_names, fixed = TRUE))
  
  if (intercept_in_varnames) {
    intercept_draws <- draws[, 1]
    coef_draws <- draws[, -1, drop = FALSE]
    intercept_name <- var_names[1]
    coef_names <- var_names[-1]
  } else if (n_beta == n_varnames) {
    intercept_draws <- NULL
    coef_draws <- draws
    coef_names <- var_names
  } else {
    stop("Cannot determine which betaAll component is the intercept.")
  }
  
  scale_y  <- model@scaleYScale
  center_y <- model@scaleYCenter
  scale_x  <- model@scaleScale
  center_x <- model@scaleCenter
  
  # Rescale coefficients
  coef_rescaled <- sweep(coef_draws, 2, scale_y / scale_x, "*")
  
  # Rescale intercepts using full formula for each draw
  intercept_rescaled <- intercept_draws * scale_y + center_y - as.vector(coef_rescaled %*% center_x)
  
  # Combine all rescaled draws
  all_rescaled <- cbind(intercept_rescaled, coef_rescaled)
  colnames(all_rescaled) <- var_names
  
  # Compute summary statistics
  summary_list <- lapply(as.data.frame(all_rescaled), function(x) {
    list(
      Estimate = mean(x),
      Median = median(x),
      SD = sd(x),
      Cred_Interval_Min = quantile(x, lower),
      Cred_Interval_Max = quantile(x, upper)
    )
  })
  
  # Convert to data.frame
  summary_df <- do.call(rbind, lapply(summary_list, as.data.frame))
  summary_df$Parameter <- names(summary_list)
  
  summary_df$Parameter <- rownames(summary_df)
  rownames(summary_df) <- NULL
  
  # Reorder columns
  summary_df <- summary_df[, c("Parameter", "Estimate", "Median", "SD", "Cred_Interval_Min", "Cred_Interval_Max")] %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  return(summary_df)
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
      joinRegressionSign(signData = models[[name]] %>%
                           extract_coeff_from_model() %>%
                           extractRegressionSign()) %>%
      prefixNameAsColumn(name = "Model", value = name)
  })
  
  c(list(global = globalImportance), modelBasedImportance)
}

joinRegressionSign <- function(importance, signData) {
  importance %>%
    dplyr::left_join(signData, by = "Variable")
}
