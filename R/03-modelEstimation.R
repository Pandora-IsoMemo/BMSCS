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
      importDataUI(ns("modelImport"), label = "Import Model"),
      tags$br(),
      downloadModelUI(ns("modelDownload"), label = "Download Model"),
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
      tagList(
        fluidRow(column(
          width = 3,
          shinyTools::dataExportButton(ns("exportAllModelOutput"),
                                       label = "Export summaries of all models")
        ),
        column(
          width = 9,
          helpText("Note: The export contains output of tabs 'Model Evaluation', 'Model Summary', 'Model Diagnostics', 'Durbin-Watson Test', 'Variable Importance'")
        )),
        tags$hr(),
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
      ))
  )
}

#' @rdname shinyModule
#' @export
modelEstimation <- function(input, output, session, data) {
  ns <- session$ns
  
  observe({
    updateSelectizeInput(session, "x", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xCategorical", choices = names(data()), selected = "")
    updatePickerInput(session, "y", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xUnc", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xCatUnc", choices = names(data()), selected = "")
    updatePickerInput(session, "yUnc", choices = names(data()), selected = "")
  }, priority = 100) %>%
    bindEvent(data())
    
  allSummaries <- callModule(modelSummary, "modelSummary", model = m, modelAVG = m_AVG)
  allDiagnostics <- callModule(modelDiagnostics, "modelDiagnostics", model = m, nChains = input$nChains)
  allICData <- callModule(modelEvaluation, "modelEvaluation", model = m)
  callModule(modelPredictions, "modelPredictions", model = m, data = data, modelAVG = m_AVG)
  callModule(modelParameters, "modelParameters", model = m, modelAVG = m_AVG)
  callModule(modelPredictionsCustom, "modelPredictionsCustom", model = m, modelAVG = m_AVG)
  callModule(modelROC, "modelROC", model = m, data = data, modelAVG = m_AVG)
  allDW <- callModule(modelDW, "modelDW", model = m, data = data, modelAVG = m_AVG)
  callModule(modelVariables, "modelVariables", model = m, data = data, modelAVG = m_AVG)
  allVariableImportance <- callModule(modelVariablesImp, "modelVariablesImp", model = m, modelAVG = m_AVG)
  
  shinyTools::dataExportServer("exportAllModelOutput",
                               dataFun = reactive(function() {
                                 if (length(m()) == 0) return(NULL)
                                 # export list of dataframes:
                                 list(
                                   `Model Evaluation` = allICData(),
                                   `Model Summary` = allSummaries(),
                                   `Model Diagnostics` = allDiagnostics(),
                                   `Durbin-Watson Test` = allDW(),
                                   `Variable Importance` = allVariableImportance()
                                 )
                               }),
                               filename = "all_model_output")
  
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
  
  m <- reactiveVal()
  
  observe({
    if (length(m()) == 0) {
      shinyjs::disable(ns("modelAvg"), asis = TRUE)
    } else {
      shinyjs::enable(ns("modelAvg"), asis = TRUE)
    }
  }) %>%
    bindEvent(m(), ignoreNULL = FALSE)
  
  # MODEL DOWN- / UPLOAD ----
  modelsForDownload <- reactive({
    if (length(m()) == 0) return(NULL)
    
    allModels <- m()
    if (length(m_AVG()) != 0) {
      # add average model if exists
      allModels$models <- c(allModels$models, m_AVG())
    } 
    
    allModels
  })
  
  modelNotes <- reactiveVal(NULL)
  downloadModelServer("modelDownload",
                      dat = data,
                      inputs = input,
                      model = modelsForDownload,
                      rPackageName = config()[["rPackageName"]],
                      fileExtension = config()[["fileExtension"]],
                      helpHTML = getHelp(id = ""),
                      modelNotes = modelNotes,
                      triggerUpdate = reactive(TRUE))
  
  uploadedModel <- importDataServer("modelImport",
                                    importType = "model",
                                    ckanFileTypes = config()[["ckanModelTypes"]],
                                    ignoreWarnings = TRUE,
                                    defaultSource = config()[["defaultSourceModel"]],
                                    fileExtension = config()[["fileExtension"]],
                                    rPackageName = config()[["rPackageName"]])
  
  observe(priority = 100, {
    req(length(uploadedModel()) > 0, uploadedModel()[[1]][["data"]])
    
    ## update data ----
    data(uploadedModel()[[1]][["data"]])
    ## reset and update notes
    modelNotes("")
    modelNotes(uploadedModel()[[1]][["notes"]])
  }) %>%
    bindEvent(uploadedModel())
  
  observe(priority = 50, {
    req(length(uploadedModel()) > 0, uploadedModel()[[1]][["inputs"]])
    
    ## update inputs ----
    uploadedInputs <- uploadedModel()[[1]][["inputs"]]
    inputIDs <- names(uploadedInputs)
    inputIDs <- inputIDs[inputIDs %in% names(input)]
    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedInputs[[inputIDs[i]]]) )
    }
    
    ## update model ----
    modelNames <- names(uploadedModel()[[1]][["model"]][["models"]])
    # extract average model if exists
    indexAvgModel <- grepl(pattern = "model_average", modelNames)
    if (any(indexAvgModel)) {
      # extract avg model
      modelObject <- uploadedModel()[[1]][["model"]]
      avgModel <- modelObject$models[modelNames[indexAvgModel]]
      
      # remove avg model from list of models
      modelObject$models[[modelNames[indexAvgModel]]] <- NULL
      
      # load single models
      m(modelObject)
      # load average model
      m_AVG(avgModel)
    } else {
      m(uploadedModel()[[1]][["model"]])
    }
  }) %>%
    bindEvent(uploadedModel())
  
  # RUN MODEL ----
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
    
    if (is.null(prepData$dataModel) || is.null(model)) {
      m(NULL)
      return()
    } 
    
    req(model)
    names(model$models) <- prepModelNames(model$models)
    # if(any(sapply(1:length(model), function(x) is.null(model[[x]])))){
    #   browser()
    # }
    fits <- withProgress({
      getModelFits(model$models, 
                   y = prepData$dataModel[, input$y], 
                   newdata = prepData$dataModel)
    }, value = 0.8, message = "Evaluate Models")
    
    m(list(models = model$models, fits = fits, dependent = input$y, variableData = model$variableData))
  }) %>%
    bindEvent(input$run)
  
  # RUN AVG MODEL ----
  m_AVG <- reactiveVal()
  observe({
    req(m())
    weights <- get_model_weights(m()$fits, measure = input$wMeasure) %>%
      tryCatchWithWarningsAndErrors()
    req(!is.null(weights))
    model_avg <- withProgress({list(
      get_avg_model(m()$models, weights) %>%
        tryCatchWithWarningsAndErrors()
    )}, value = 0, message = "Calculate model average")
    names(model_avg) <- paste0("model_average_", input$wMeasure)
    
    m_AVG(model_avg)
  }) %>%
    bindEvent(input$modelAvg)
  
  observe({
    req(m())
    if(m()$models[[1]]@type == "linear"){
      hideTab(inputId = "modTabs", target = "ROC")
    } else {
      showTab(inputId = "modTabs", target = "ROC")
    }
  })
}
