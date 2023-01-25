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
      selectizeInput(ns("x"), "Independent (X) numeric variables", choices = NULL, multiple = TRUE, selected = NULL),
      selectizeInput(ns("xCat"), "Independent (X) categorical variables (optional)", choices = NULL, multiple = TRUE, selected = NULL),
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
      radioButtons(ns("regType"), label = "Regression type", choices = c("linear", "logistic"), selected = "linear"),
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
    updateSelectizeInput(session, "xCat", choices = names(data()), selected = "")
    updatePickerInput(session, "y", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xUnc", choices = names(data()), selected = "")
    updateSelectizeInput(session, "xCatUnc", choices = names(data()), selected = "")
    updatePickerInput(session, "yUnc", choices = names(data()), selected = "")
  })

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
    xCat <- ""
    if(!is.null(input$xCat) && any(input$xCat != "")){
      xVars <- c(xVars, input$xCat)
      xCat <- input$xCat
    }

    FORMULA <- generateFormula(input$y, xVars)
    FORMULA <- createFormula(formula = FORMULA,
                  maxExponent = input$maxExp,
                  inverseExponent = input$inverseExp,
                  interactionDepth = input$interactionDepth,
                  intercept = input$intercept,
                  categorical = xCat)
    
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

  m <- eventReactive(input$run, {
    if(is.null(input$y) || input$y == ""){
      shinyjs::alert("Please select an dependent variable")
      return(NULL)
    }
    
    if(is.null(input$x) || all(input$x == "")){
      shinyjs::alert("Please select an independent variable(s)")
      return(NULL)
    }
    xVars <- input$x
    if(!is.null(input$xCat) && any(input$xCat != "")){
      xVars <- c(xVars, input$xCat)
      xCat <- input$xCat
    } else {
      xCat <- ""
    }
    FORMULA <- generateFormula(input$y, xVars)
    dataModel <- data()

    if(all(is.na(dataModel[, input$y]))){
      shinyjs::alert("Dependent variable has no numeric values")
      return(NULL)
    }
    
    if(any(apply(dataModel[, input$x, drop = FALSE], 2, function(k) all(is.na(as.numeric(k)))))){
      shinyjs::alert("At least one x variable has no numeric values")
      return(NULL)
    }
    
    dataModel[, input$x] <- as.data.frame(sapply(dataModel[, input$x], as.numeric)) 

    if(!is.null(input$xUnc)){
      xUnc <- dataModel[, input$xUnc, drop = FALSE]
      names(xUnc) <- input$x
      if(any(apply(dataModel[, input$xUnc, drop = FALSE], 2, function(k) all(as.numeric(is.na(k)))))){
        shinyjs::alert("At least one x uncertainty variable has no numeric values")
        return(NULL)
      }
      if(length(input$xUnc) != length(input$x)){
        shinyjs::alert("x-uncertainty variables must have same length as x-variables")
        return(NULL)
      }
    } else {
      xUnc <- NULL
    }
    
    if(!is.null(input$xCatUnc)){
      xCatUnc <- dataModel[, input$xCatUnc, drop = FALSE]
      names(xCatUnc) <- input$xCat
      if(any(apply(dataModel[, input$xCatUnc, drop = FALSE], 2, function(k) all(as.numeric(is.na(k)))))){
        shinyjs::alert("At least one x categorical uncertainty variable has no numeric values")
        return(NULL)
      }
      if(length(input$xCatUnc) != length(input$xCat)){
        shinyjs::alert("x-missclassification variables must have same length as x-categorical variables")
        return(NULL)
      }
      
    } else {
      xCatUnc <- NULL
    }
    if(xCat[1] == ""){
      mVars <- c(input$x)
    } else {
      mVars <- c(input$x, xCat)
    }
    missingVars <- names(which(sapply(mVars, function(x) any(is.na(dataModel[, x])))))
    if(length(missingVars) > 0){
      dataModel2 <- dataModel
      dataModel <- na.omit(dataModel)
      nMissing <- nrow(dataModel2) - nrow(dataModel)
      shinyjs::alert(paste0("Missing values found in following variables: ", paste(missingVars, collapse = ", "), "; ", nMissing, " observations deleted."))
    }

    if(!is.numeric(dataModel[, input$y])){
      dataModel[, input$y] <- as.numeric(dataModel[, input$y])
    }
    
    if(!is.null(input$yUnc)){
      yUnc <- as.numeric(as.vector(dataModel[, input$yUnc]))
      if(all(is.na(yUnc))){
        shinyjs::alert("Dependent variable uncertainty has no numeric values")
        return(NULL)
      }
    } else {
      yUnc <- rep(0, nrow(dataModel))
    }
    
    if(input$regType == "logistic" & any(!(dataModel[, input$y] %in% c(0,1)))){
      shinyjs::alert("Dependent variable must have only 0 and 1 values if logistic regression is selected.")
      return(NULL)
    }
    set.seed(1234)
    
    model <- withProgress({constrSelEst(
                formula = FORMULA,
                mustInclude = input$mustInclude, 
                mustExclude = input$mustExclude,
                 maxExponent = input$maxExp,
                 inverseExponent = input$inverseExp,
                 interactionDepth = input$interactionDepth,
                
                 categorical = xCat,
                 ar1 = input$ar1,
                 intercept = input$intercept,
                 constraint_1 = input$constraint, data = dataModel,
                 xUncertainty = xUnc,
                 xCatUncertainty = xCatUnc,
                 yUncertainty = yUnc, maxNumTerms = input$maxTerms,
                 type = input$regType,
                 scale = input$scale,
                 chains = input$nChains,
                 burnin = input$burnin,
                 iterations = input$iter,
                 shiny = TRUE,
                 imputeMissings = input$imputeMissings)}, value = 0, message = "Calculation in progess",
                 detail = 'This may take a while')
    names(model$models) <- prepModelNames(model$models)
    # if(any(sapply(1:length(model), function(x) is.null(model[[x]])))){
    #   browser()
    # }
    fits <- withProgress({getModelFits(model$models, y = dataModel[, input$y], newdata = dataModel)}, value = 0.8, message = "Evaluate Models")
    
    return(list(models = model$models, fits = fits, dependent = input$y, variableData = model$variableData))
  })
  
  m_AVG <- eventReactive(input$modelAvg, {
      req(m())
      weights <- get_model_weights(m()$fits, measure = input$wMeasure)
      req(!is.null(weights))
      model_avg <- withProgress({list(get_avg_model(m()$models, weights))}, value = 0, message = "Calculate model average")
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

