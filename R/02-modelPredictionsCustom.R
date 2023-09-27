modelPredictionsCustomTab <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Custom Model Predictions",
    value = "predictionTabCustom",
    h4("Import custom data set for model prediction:"),
    importDataUI(ns("dataCustom"), "Import Data"),
    checkboxInput(ns("yProv"), "Provide optional dependent y-variable:", FALSE),
    conditionalPanel(
      condition = "input.yProv == true",
      ns = ns,
    selectInput(ns("yNewType"), "Type of provided y-variable",
                choices = c("Mean", "Mean + SD", "Interval"), multiple = FALSE, selected = "Mean"),
    conditionalPanel(
      condition = "input.yNewType == 'Mean + SD' || input.yNewType == 'Mean'",
      ns = ns,
      selectInput(ns("yNewMean"), "Select y variable mean for comparison (optional)",
                  choices = NULL, multiple = FALSE, selected = NULL)
    ),
    conditionalPanel(
      condition = "input.yNewType == 'Mean + SD'",
      ns = ns,
      selectInput(ns("yNewSD"), "Select y variable sd for comparison (optional)",
                  choices = NULL, multiple = FALSE, selected = NULL)
    ),
    conditionalPanel(
      condition = "input.yNewType == 'Interval'",
      ns = ns,
      selectInput(ns("yNewI1"), "Select y variable lower interval for comparison (optional)",
                  choices = NULL, multiple = FALSE, selected = NULL),
      selectInput(ns("yNewI2"), "Select y variable upper intervalfor comparison (optional)",
                  choices = NULL, multiple = FALSE, selected = NULL),
      sliderInput(ns("quantileInt"), "Select quantile for y-variable intervals",
                  min = 0.5, max = 0.999, step = 0.001, value = 0.95)
    )),
    selectInput(ns("modelSelection"), "Select Model", choices = ""),
    sliderInput(ns("quantilePred"), "Select quantile for prediction intervals",
                min = 0.5, max = 0.999, step = 0.001, value = 0.95
    ),
    tableOutput(ns("predCustom")),
    dataExportButton(ns("exportEstimates"))
  )
}

modelPredictionsCustom <- function(input, output, session, model, modelAVG, config) {
  datCustom <- reactiveVal(NULL)
  
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
    req(modelAVG())
    updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
  })
  
  observe({
    req(datCustom())
    updateSelectInput(session, "yNewMean", choices = c("", names(datCustom())), selected = "")
    updateSelectInput(session, "yNewSD", choices = c("", names(datCustom())), selected = "")
    updateSelectInput(session, "yNewI1", choices = c("", names(datCustom())), selected = "")
    updateSelectInput(session, "yNewI2", choices = c("", names(datCustom())), selected = "")
  })

  importedDataCustom <- importDataServer(
    "dataCustom",
    defaultSource = config$defaultSourceData,
    rPackageName = config$rPackageName
  )
  
  observeEvent(importedDataCustom(), {
    req(length(importedDataCustom()) > 0)
    datCustom(importedDataCustom()[[1]])
  })

  dataFun <- reactive({
    req(model())
    req(datCustom())
    
    function() { 
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
      modelVars <- all.vars(mPar@formula)[-1]
      
      if(!all(modelVars %in% colnames(datCustom()))){
        shinyjs::alert("Not all model variables supplied in uploaded data")
        return(NULL)
      }
      predictions <- t(BMSC::predict(mPar, datCustom(), samples = TRUE))
      resData <- data.frame(mean = colMeans(predictions),
                                  median = apply(predictions, 2, median),
                                  sd = apply(predictions, 2, sd),
                                  lowerQuantile = apply(predictions, 2, quantile, (1 - input$quantilePred) / 2),
                                  upperQuantile = apply(predictions, 2, quantile,  1 - (1 - input$quantilePred) / 2))
      
      if(input$yProv == TRUE){
        if(input$yNewType == "Mean"){
          if(input$yNewMean %in% names(datCustom())){
          predY <- datCustom()[, input$yNewMean]
          lowerQ <- sapply(1:ncol(predictions), function(i) quantile((predY[i] - predictions[, i]), (1 - input$quantilePred) / 2))
          upperQ <- sapply(1:ncol(predictions), function(i) quantile((predY[i] - predictions[, i]), 1 - (1 - input$quantilePred) / 2))
          resData <- data.frame(resData, y_Mean = datCustom()[, input$yNewMean],
                                Significance = ((lowerQ < 0 & upperQ < 0) | (lowerQ > 0 & upperQ > 0)))
          }
        }
        if(input$yNewType == "Mean + SD"){
          if(input$yNewMean %in% names(datCustom()) && input$yNewSD %in% names(datCustom())){
            predY <- sapply(1:nrow(datCustom), function(i) rnorm(nrow(predictions),
                                                                 datCustom()[i, input$yNewMean], datCustom()[i, input$yNewSD]))
            lowerQ <- sapply(1:ncol(predictions), function(i) quantile((predY[i] - predictions[, i]), (1 - input$quantilePred) / 2))
            upperQ <- sapply(1:ncol(predictions), function(i) quantile((predY[i] - predictions[, i]), 1 - (1 - input$quantilePred) / 2))
            resData <- data.frame(resData, y_Mean = datCustom()[, input$yNewMean],
                                  y_SD = datCustom()[, input$yNewSD],
                                  Significance = ((lowerQ < 0 & upperQ < 0) | (lowerQ > 0 & upperQ > 0)))
            
          }
        }
        if(input$yNewType == "Interval"){
          if(input$yNewC1 %in% names(datCustom()) && input$yNewC2 %in% names(datCustom())){
            means <- (datCustom()[, input$yNewI1] + datCustom()[, input$yNewI2]) / 2
            sds <- (datCustom()[, input$yNewI1] - datCustom()[, input$yNewI2]) / qnorm(1 - (1 - input$quantileInt) / 2)
            predY <- sapply(1:nrow(datCustom), function(i) rnorm(nrow(predictions), means, sds))
            lowerQ <- sapply(1:ncol(predictions), function(i) quantile((predY[i] - predictions[, i]), (1 - input$quantilePred) / 2))
            upperQ <- sapply(1:ncol(predictions), function(i) quantile((predY[i] - predictions[, i]), 1 - (1 - input$quantilePred) / 2))
            resData <- data.frame(resData, y_lowerCI = datCustom()[, input$yNewI1],
                                  y_upperCI = datCustom()[, input$yNewI2],
                                  Significance = ((lowerQ < 0 & upperQ < 0) | (lowerQ > 0 & upperQ > 0)))
          }
        }
      }
      resData
    }
  })
  
  output$predCustom <- renderTable(dataFun()(), bordered = TRUE,
                                 rownames = FALSE, colnames = TRUE)
  
  callModule(dataExport, "exportEstimates", data = dataFun, filename = "predictionsCustom")
}