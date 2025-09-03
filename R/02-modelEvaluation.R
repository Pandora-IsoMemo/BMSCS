modelEvaluationTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Model Evaluation",
        value = "modelEvaluationTab",
        plotOutput(ns("plot")),
        fluidRow(column(
          4,
          radioButtons(ns("ic"), "Information / Cross-Validation Error criterion",
                       choices = c("Loo", "WAIC", "AIC", "AICc", "BIC", "logLik", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "MallowsCP"), width = "100%"
          ),
          sliderInput(ns("thresholdSE"), "Standard error threshold for best model selection", min = 0, max = 3, step = 0.1, value = 1, width = "100%"),
          h5("Table of evaluation measure"),
          tableOutput(ns("evalData")),
          shinyTools::dataExportButton(ns("exportModelEvaluationData"))
        ),
        column(4,
               sliderInput(ns("eAxis"), label = "Axis label font size", min = 1, max = 24, value = 12, width = "100%"),
               sliderInput(ns("eAngle"), label = "x-Axis label angle", min = 3, max = 60, value = 12, width = "100%"),
               plotExportButton(ns("exportPlot"))
               ),
        column(4,
               shinyTools::customPointsUI(
                 id = ns("evaluationPlotCustomPoints"),
                 plot_type = "ggplot"
               )
        ))
    )
}

modelEvaluation <- function(input, output, session, model) {
    observe({
        req(model())

        if (model()$models[[1]]@type == "linear") {
            updateRadioButtons(session, "ic",
                choices = c("Loo", "WAIC", "AIC", "AICc", "BIC", "logLik", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "MallowsCP")
            )
        }
        if (model()$models[[1]]@type == "logistic") {
            updateRadioButtons(session,
                "ic",
                choices = c("Loo", "WAIC", "AIC", "AICc", "BIC", "logLik", "Rsq", "Bayes_Rsq", "AUC", "df", "nagelkerke")
            )
        }
    })

  allICData <- reactiveVal()
  observe({
    req(model())
    
    ICList <- names(model()$fits)
    thisICData <- lapply(ICList,
                       function(x)
                         getICData(ic = x,
                                   allFits = model()$fits,
                                   modelNames = names(model()$models),
                                   withColumnICName = TRUE)
    ) %>% 
      bindAllResults(addEmptyRow = TRUE) %>%
      shinyTryCatch(errorTitle = "Extracting model results failed")
    
    allICData(thisICData)
  })
  
  
  baseplot <- reactive({
    if (length(model()) == 0)
      return(NULL)
    
    plotModelFit(
      model()$models,
      fits = model()$fits,
      thresholdSE = input$thresholdSE,
      markBestModel = TRUE,
      ic = input$ic,
      tAngle = input$eAngle,
      aSize = input$eAxis
    )
  })
  
  x_choices <- reactive({
    if (length(model()) == 0)
      return(NULL)
    p <- baseplot()
    # group : p$data$model
    choices <- p$data$model |> unique() |> as.character()
    attr(choices, "x") <- "model"
    
    return(choices)
  })
  
  modelParamPlotCustPoints <- shinyTools::customPointsServer("evaluationPlotCustomPoints",
                                                             plot_type = "ggplot",
                                                             x_choices = x_choices)
  
  
  plotFun <- reactive({
    function() {
      baseplot() |> 
        shinyTools::addCustomPointsToGGplot(modelParamPlotCustPoints()) |>
        plot() |>
        shinyTryCatch(errorTitle = "Plotting failed")
    }
  })

  plotExportServer("exportPlot", plotFun = plotFun)

    output$plot <- renderPlot({
      validate(need(input$ic %in% names(model()$fits), message = sprintf("No data available for '%s'. Please re-run the model to get results.", input$ic)))
      
      plotFun()()
    })
    
    dataFun <- reactive({
        function() {
          if (length(model()) == 0 || !(input$ic %in% names(model()$fits))) return(NULL)
          # for csv / excel - export:
          getICData(allFits = model()$fits, modelNames = names(model()$models), ic = input$ic)
        }
    })
    
    output$evalData <- renderTable({
      validate(need(input$ic %in% names(model()$fits), message = sprintf("No data available for '%s'. Please re-run the model to get results.", input$ic)))
      
      dataFun()()
      }, 
      bordered = TRUE,
      rownames = FALSE, 
      colnames = TRUE)
    
    shinyTools::dataExportServer("exportModelEvaluationData", dataFun = dataFun, filename = "evaluation")
    
    return(allICData)
}

#' Get Data of IC
#' 
#' @param allFits (list) list of model$fits objects
#' @param modelNames (character) name of all models
#' @param ic (character) name of information criterion, e.g. \code{"AUC", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "logLik", "nagelkerke", "Loo", "WAIC"}
#' @param withColumnICName (logical) if TRUE, add a separate first column "IC_name" containing the
#'  name of ic. Useful, for \code{bind_rows} over several ic values
getICData <- function(allFits, modelNames, ic, withColumnICName = FALSE) {
  fits <- allFits[[ic]]
  
  # set colname of IC column
  if (withColumnICName) {
    colnameIC <- "IC_value"
  } else{
    colnameIC <- ic
  }
  
  # return empty data.frame
  if (is.null(fits)) {
    emptyRes <- data.frame(Model = modelNames, Fits = NA, Rank = NA)
    colnames(emptyRes)[2] <- colnameIC
    
    if (withColumnICName) {
      emptyRes <- emptyRes %>%
        prefixNameAsColumn(name = "IC_name", value = ic)
    }
    
    return(emptyRes)
  }
  
  # return IC values
  if (ic == "Loo") {
    fits <- sapply(fits, function(x) x$estimates["elpd_loo","Estimate"])
  }
  if (ic == "WAIC") {
    fits <- sapply(fits, function(x) x$estimates["elpd_waic", "Estimate"])
  }
  fits <- data.frame(fits)
  names(fits) <- colnameIC
  
  if(ic %in% c("AUC", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "logLik", "nagelkerke", "Loo", "WAIC")){
    ranks <- as.integer(round(rank(-fits[,1]), 0))
  } else {
    ranks <- as.integer(round(rank(fits[,1]), 0))
  }
  res <- data.frame(Model = modelNames, fits, Rank = ranks)
  
  if (withColumnICName) {
    res <- res %>%
      prefixNameAsColumn(name = "IC_name", value = ic)
  }
  
  res
}


prefixNameAsColumn <- function(df, name, value) {
  df[[name]] <- value
  df[, c(ncol(df), 1:(ncol(df)-1))]
}
