modelSummaryTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Model Summary",
        value = "summaryTab",
        selectInput(ns("modelSelection"), "Select model", choices = ""),
        sliderInput(ns("quantileInt"), "Select quantile for credible intervals",
            min = 0.5, max = 0.999, step = 0.001, value = 0.95
        ),
        verbatimTextOutput(ns("summary")),
        textExportButton(ns("exportText"))
    )
}

modelSummary <- function(input, output, session, model, modelAVG) {
    observe({
        req(model())
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
        req(modelAVG())
        updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
    })
    
  allSummaries <- reactiveVal()
  observe({
    req(model())
    
    thisSummaries <- extractAllSummaries(
      allModels = model()$models,
      cLevel = input$quantileInt
    ) %>% 
      bindAllResults(addEmptyRow = TRUE)
    
    allSummaries(thisSummaries)
  })
    
    printFun <- reactive({
        req(model())
        req((input$modelSelection %in% names(model()$models)) || (input$modelSelection %in% names(modelAVG())))
        
        function() {
            if((input$modelSelection %in% names(model()$models))){
                print(model()$models[[input$modelSelection]], cLevel = input$quantileInt)
            } else {
                print(modelAVG()[[input$modelSelection]], cLevel = input$quantileInt)
            }
        }
    })

    output$summary <- renderPrint({
        req(model())
        printFun()()
    })

    callModule(textExport, "exportText", printFun = printFun, filename = "summary")
    
    return(allSummaries)
}

extractAllSummaries <- function(allModels, cLevel, asDataFrame = TRUE) {
  modelNames <- names(allModels)
  names(modelNames) <- modelNames
  
  lapply(modelNames, function(x) {
    res <- capture.output({print(allModels[[x]], cLevel = cLevel)})
    if (asDataFrame) {
      res <- res %>%
        as.data.frame()
      colnames(res) <- "Model Summary"
      
      res <- res %>%
        prefixNameAsColumn(name = "model", value = x)
    }
    
    res
  })
}

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
