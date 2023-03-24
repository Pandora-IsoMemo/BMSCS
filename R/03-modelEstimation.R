#' @rdname shinyModule
#' @export
modelEstimationUI <- function(id, title = "") {
  ns <- NS(id)
  tabPanel(
    title = title,
    id = id,
    value = id,
    useShinyjs(),
    sidebarPanel(
      style = "position:fixed; width:23%; max-width:500px; overflow-y:auto; height:88%",
      width = 3,
      uploadModelUI(ns("modelUpload"), label = NULL),
      downloadModelUI(ns("modelDownload"), label = NULL),
      hr(style = "border-top: 1px solid #000000;"),
      selectizeInput(ns("x"), "Independent (X) numeric variables", choices = NULL, multiple = TRUE, selected = NULL),
      selectizeInput(ns("xCategorical"), "Independent (X) categorical variables (optional)", choices = NULL, multiple = TRUE, selected = NULL),
      pickerInput(ns("y"), "Dependent (Y) variable", choices = NULL, multiple = FALSE, selected = NULL),
      sliderInput(ns("interactionDepth"), label = "Interaction depth",
                  min = 1, max = 3, step = 1, value = 1),
      sliderInput(ns("maxExp"), label = "Max exponent",
                  min = 1, max = 3, step = 1, value = 1),
      sliderInput(ns("inverseExp"), label = "Max inverse exponent",
                  min = 1, max = 3, step = 1, value = 1),
      checkboxInput(ns("intercept"), label = "Include intercept", value = TRUE),
      checkboxInput(ns("ar1"), label = "Include error autocorrelation term", value = FALSE),
      checkboxInput(ns("constraint"), label = "Constrain regression parameters to 1", value = FALSE),
      pickerInput(ns("mustInclude"), "Must include variables (optional)", choices = NULL, multiple = TRUE, selected = NULL, options = list(`actions-box` = TRUE)),
      pickerInput(ns("mustExclude"), "Must exclude variables (optional)", choices = NULL, multiple = TRUE, selected = NULL, options = list(`actions-box` = TRUE)),
      selectizeInput(ns("xUnc"), "X numerical variables uncertainty (optional)", choices = NULL, multiple = TRUE, selected = NULL),
      selectizeInput(ns("xCatUnc"), "X categorical variables misclassification rate (optional)", choices = NULL, multiple = TRUE, selected = NULL),
      pickerInput(ns("yUnc"), "Dependent variable uncertainty (optional)", choices = NULL, multiple = FALSE, selected = NULL),
      radioButtons(ns("regType"), label = "Regression type", choices = c("linear", "logistic"), selected = "linear", inline = TRUE),
      sliderInput(ns("maxTerms"), label = "Max number of terms in formula",
                  min = 2, max = 200, step = 1, value = 8),
      conditionalPanel(
        condition = "input.constraint == false",
        checkboxInput(ns("scale"), label = "Scale variables to mean 0 and sd 1 (recommended)", value = TRUE),
        ns = ns
      ),
      checkboxInput(ns("imputeMissings"), label = "Impute missing values (multiple imputation via \"pmm\" method)", value = TRUE),
      sliderInput(ns("nChains"), label = "Number of MCMC chains",
                  min = 1, max = 8, step = 1, value = 3),
      sliderInput(ns("burnin"), label = "Number of burnin iterations",
                  min = 200, max = 3000, step = 100, value = 300),
      sliderInput(ns("iter"), label = "Number of MCMC iterations",
                  min = 100, max = 20000, step = 100, value = 500),
      actionButton(ns("run"), "Run model"),
      hr(style = "border-top: 1px solid #000000;"),
      h5("Model average (applicable after model run)"),
      selectInput(ns("wMeasure"), label = "Weighting measure for model averaging",
                  choices = c("Loo", "WAIC", "AIC", "AICc", "BIC", "logLik")),
      actionButton(ns("modelAvg"), "Create model average")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("modTabs"),
        modelEvaluationTab(ns("modelEvaluation")),
        modelSummaryTab(ns("modelSummary")),
        modelDiagnosticsTab(ns("modelDiagnostics")),
        modelPredictionsTab(ns("modelPredictions")),
        modelParametersTab(ns("modelParameters")),
        modelROCTab(ns("modelROC")),
        modelDWTab(ns("modelDW")),
        modelPredictionsCustomTab(ns("modelPredictionsCustom")),
        modelVariablesTab(ns("modelVariables")),
        modelVariablesImpTab(ns("modelVariablesImp"))
      )
    )
  )
}

#' @rdname shinyModule
#' @export
modelEstimation <- function(input, output, session, data) {
  
  observe({
    updateSelectizeInput(session, "x", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xCategorical", choices = names(data()), selected = "")
    updatePickerInput(session, "y", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xUnc", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xCatUnc", choices = names(data()), selected = "")
    updatePickerInput(session, "yUnc", choices = names(data()), selected = "")
  }, priority = 100) %>%
    bindEvent(data())
    
  callModule(modelSummary, "modelSummary", model = m, modelAVG = m_AVG)
  callModule(modelDiagnostics, "modelDiagnostics", model = m, nChains = input$nChains)
  callModule(modelEvaluation, "modelEvaluation", model = m)
  callModule(modelPredictions, "modelPredictions", model = m, data = data, modelAVG = m_AVG)
  callModule(modelParameters, "modelParameters", model = m, modelAVG = m_AVG)
  callModule(modelPredictionsCustom, "modelPredictionsCustom", model = m, modelAVG = m_AVG)
  callModule(modelROC, "modelROC", model = m, data = data, modelAVG = m_AVG)
  callModule(modelDW, "modelDW", model = m, data = data, modelAVG = m_AVG)
  callModule(modelVariables, "modelVariables", model = m, data = data, modelAVG = m_AVG)
  callModule(modelVariablesImp, "modelVariablesImp", model = m, modelAVG = m_AVG)
  
  formulaParts <- reactive({
    if(!is.null(input$y) && !is.null(input$x) && input$y != "" && any(input$x != "")){
    xVars <- input$x
    xCategorical <- ""
    if(!is.null(input$xCategorical) && any(input$xCategorical != "")){
      xVars <- c(xVars, input$xCategorical)
      xCategorical <- input$xCategorical
    }

    FORMULA <- generateFormula(input$y, xVars)
    FORMULA <- createFormula(formula = FORMULA,
                  maxExponent = input$maxExp,
                  inverseExponent = input$inverseExp,
                  interactionDepth = input$interactionDepth,
                  intercept = input$intercept,
                  categorical = xCategorical)
    
    ret <- gsub('[\n ]', '', strsplit(strsplit(as.character(FORMULA)[3], "~")[[1]], " \\+ ")[[1]])
    return(ret)
    } else {
      return("")
    }
  })
  
  observe({
    updatePickerInput(session, "mustInclude", choices = formulaParts(), selected = "")
    updatePickerInput(session, "mustExclude", choices = formulaParts(), selected = "")
  })

  # MODEL DOWN- / UPLOAD ----
  # no download of model output since it is too large to be uploadad again
  downloadModelServer("modelDownload",
                      dat = data, 
                      inputs = input, 
                      #model = rawModel,
                      model = NULL)
  
  uploadedData <- uploadModelServer("modelUpload")
  
  observe(priority = 500, {
    ## update data ----
    data(uploadedData$data)
  }) %>%
    bindEvent(uploadedData$data)
  
  observe(priority = -100, {
    ## update inputs ----
    inputIDs <- names(uploadedData$inputs)
    for (i in 1:length(uploadedData$inputs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedData$inputs[[inputIDs[i]]]) )
      if (inputIDs[i] == "modelDownload-exportNotes") {
        print(uploadedData$inputs[[inputIDs[i]]])
      }
    }
  }) %>%
    bindEvent(uploadedData$inputs)
  
  # # currently model upload is not possible since the model output is to large to be uploaded again
  # # upload limit exceeded
  #
  # observeEvent(uploadedData$model, priority = -200, {
  # ## update model ----
  #   rawModel(uploadedData$model)
  # })
  
  # RUN MODEL ----
  dataModel <- reactiveVal()
  rawModel <- reactiveVal()
  
  observe({
    prepData <- data() %>%
      prepareData(in_x = input$x,
                  in_xUnc = input$xUnc,
                  in_y = input$y,
                  in_yUnc = input$yUnc,
                  in_xCategorical = input$xCategorical,
                  in_xCatUnc = input$xCatUnc,
                  in_regType = input$regType) %>%
      tryCatchWithWarningsAndErrors()
    
    if (is.null(prepData)) {
      dataModel(NULL)
      rawModel(NULL)
      }
    
    req(prepData)
    FORMULA <- generateFormula(input$y, prepData$xVars)
    
    set.seed(1234)
    model <- withProgress({
      constrSelEst(
        formula = FORMULA,
        mustInclude = input$mustInclude,
        mustExclude = input$mustExclude,
        maxExponent = input$maxExp,
        inverseExponent = input$inverseExp,
        interactionDepth = input$interactionDepth,
        categorical = prepData$xCategorical,
        ar1 = input$ar1,
        intercept = input$intercept,
        constraint_1 = input$constraint,
        data = prepData$dataModel,
        xUncertainty = prepData$xUnc,
        xCatUncertainty = prepData$xCatUnc,
        yUncertainty = prepData$yUnc,
        maxNumTerms = input$maxTerms,
        type = input$regType,
        scale = input$scale,
        chains = input$nChains,
        burnin = input$burnin,
        iterations = input$iter,
        shiny = TRUE,
        imputeMissings = input$imputeMissings
      ) %>%
        tryCatchWithWarningsAndErrors()
    },
    value = 0,
    message = "Calculation in progess",
    detail = 'This may take a while')
    
    dataModel(prepData$dataModel)
    rawModel(model)
  }) %>%
    bindEvent(input$run)
  
  m <- eventReactive(rawModel(), ignoreNULL = FALSE, ignoreInit = TRUE, { 
    dataModel <- dataModel()
    model <- rawModel()
    
    if (is.null(dataModel) || is.null(model)) return(NULL)
    
    names(model$models) <- prepModelNames(model$models)
    # if(any(sapply(1:length(model), function(x) is.null(model[[x]])))){
    #   browser()
    # }
    fits <- withProgress({getModelFits(model$models, y = dataModel[, input$y], newdata = dataModel)}, value = 0.8, message = "Evaluate Models")
    
    return(list(models = model$models, fits = fits, dependent = input$y, variableData = model$variableData))
  })
  
  m_AVG <- eventReactive(input$modelAvg, {
      req(m())
      weights <- get_model_weights(m()$fits, measure = input$wMeasure) %>%
        tryCatchWithWarningsAndErrors()
      req(!is.null(weights))
      model_avg <- withProgress({list(
        get_avg_model(m()$models, weights) %>%
          tryCatchWithWarningsAndErrors()
      )}, value = 0, message = "Calculate model average")
      names(model_avg) <- paste0("model_average_", input$wMeasure)
      return(model_avg)
  })
  
  observe({
    req(m())
    if(m()$models[[1]]@type == "linear"){
      hideTab(inputId = "modTabs", target = "ROC")
    } else {
      showTab(inputId = "modTabs", target = "ROC")
    }
  })
}
