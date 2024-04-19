modelEvaluationTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Model Evaluation",
        value = "modelEvaluationTab",
        plotOutput(ns("plot")),
        sliderInput(ns("eAxis"), label = "Axis label font size", min = 1, max = 24, value = 12),
        sliderInput(ns("eAngle"), label = "x-Axis label angle", min = 3, max = 60, value = 12),
        radioButtons(ns("ic"), "Information / Cross-Validation Error criterion",
            choices = c("Loo", "WAIC", "AIC", "AICc", "BIC", "logLik", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "MallowsCP"), width = "100%"
        ),
        sliderInput(ns("thresholdSE"), "Standard error threshold for best model selection", min = 0, max = 3, step = 0.1, value = 1),
        plotExportButton(ns("exportPlot")),
        h5("Table of evaluation measure"),
        tableOutput(ns("evalData")),
        dataExportButton(ns("exportData"))
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

    plotFun <- reactive({
        req(model())

        function() {
            plot(plotModelFit(model()$models, fits = model()$fits, thresholdSE = input$thresholdSE, markBestModel = TRUE, ic = input$ic,
                              tAngle = input$eAngle, aSize = input$eAxis))
        }
    })

    callModule(plotExport, "exportPlot", plotFun = plotFun)

    output$plot <- renderPlot({
      validate(need(input$ic %in% names(model()$fits), message = sprintf("No data available for '%s'. Please re-run the model to get results.", input$ic)))
      
      plotFun()()
    })
    
    dataFun <- reactive({
        req(model())

        function() {
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
    
    callModule(dataExport, "exportData", data = dataFun, filename = "evaluation")
}

#' Get Data of IC
#' 
#' @param ic (character) name of information criterion, e.g. \code{"AUC", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "logLik", "nagelkerke", "Loo", "WAIC"}
getICData <- function(allFits, modelNames, ic) {
  fits <- allFits[[ic]]
  if (ic == "Loo") {
    fits <- sapply(fits, function(x) x$estimates["elpd_loo","Estimate"])
  }
  if (ic == "WAIC") {
    fits <- sapply(fits, function(x) x$estimates["elpd_waic", "Estimate"])
  }
  fits <- data.frame(fits)
  names(fits) <- ic
  if(ic %in% c("AUC", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "logLik", "nagelkerke", "Loo", "WAIC")){
    ranks <- as.integer(round(rank(-fits[,1]), 0))
  } else {
    ranks <- as.integer(round(rank(fits[,1]), 0))
  }
  data.frame(model = modelNames, fits, rank = ranks)
}
