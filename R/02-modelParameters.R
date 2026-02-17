modelParametersTab <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Model Parameters",
    value = "parameterTab",
    selectInput(ns("modelSelection"), "Select model", choices = ""),
    plotOutput(ns("plot")),
    fluidRow(
      column(6, shinyTools::customPointsUI(
        id = ns("modelParamPlotCustomPoints"),
        plot_type = "ggplot"
      )),
      column(6,
             align = "right",
             plotExportButton(ns("exportPlot")), shinyTools::dataExportButton(ns(
        "exportModelParameterData"
      )))
    )
  )
}

modelParameters <- function(input, output, session, model, modelAVG) {
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
    req(modelAVG())
    updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
  })
  
  baseplot <- reactive({
    if (length(model()) == 0 ||
        !((input$modelSelection %in% names(model()$models)) ||
          (input$modelSelection %in% names(modelAVG()))))
      return(NULL)
    
    if ((input$modelSelection %in% names(model()$models))) {
      mPar <- model()$models[[input$modelSelection]]
    } else {
      mPar <- modelAVG()[[input$modelSelection]]
    }
    parameterValues <- extract(mPar)$betaAll
    
    if (mPar@hasIntercept) {
      parameterValues[, 1] <-  mPar@scaleYCenter +
        mean(mPar@scaleYScale) * (parameterValues[, 1] -
                                    rowSums(sweep(
                                      parameterValues[, -1, drop = FALSE],
                                      2,
                                      (mPar@scaleCenter / mPar@scaleScale),
                                      '*'
                                    )))
      parameterValues[, -1] <- sweep(parameterValues[, -1, drop = FALSE], 2, (mean(mPar@scaleYScale) / mPar@scaleScale), '*')
    } else {
      parameterValues <- sweep(parameterValues, 2, (mean(mPar@scaleYScale) / mPar@scaleScale), '*')
    }
    
    parameterNames <- mPar@varNames
    
    parameterValues <- as.data.frame(parameterValues)
    names(parameterValues) <- parameterNames
    parameterValues <- gather(parameterValues)
    dataSummary <- parameterValues %>%
      group_by(.data$key) %>%
      summarise(
        # sd = sd(estimate),
        med = median(.data$value, na.rm = TRUE),
        meanEst = mean(.data$value, na.rm = TRUE),
        q68 = quantile(.data$value, 0.68,  na.rm = TRUE, names = FALSE),
        q95 = quantile(.data$value, 1 - ((1 - 0.95) / 2),  na.rm = TRUE, names = FALSE),
        q32 = quantile(.data$value, 1 - 0.68,  na.rm = TRUE, names = FALSE),
        q05 = quantile(.data$value, (1 - 0.95) / 2,  na.rm = TRUE, names = FALSE)
      ) %>%
      ungroup()
    p <- ggplot(dataSummary, aes_(x = ~ key)) +
      ylab("Estimate") + xlab("")
    
    p + geom_boxplot(
      mapping = aes_(
        lower = ~ q32,
        upper = ~ q68,
        middle = ~ med,
        ymin = ~ q05,
        ymax = ~ q95
      ),
      stat = "identity"
    ) + geom_errorbar(aes_(ymin = ~ meanEst, ymax = ~ meanEst),
                      linetype = "dashed",
                      data = dataSummary)
  })
  
  x_choices <- reactive({
    if (length(model()) == 0 ||
        !((input$modelSelection %in% names(model()$models)) ||
          (input$modelSelection %in% names(modelAVG()))))
      return(NULL)
    p <- baseplot()
    # group : p$data$key
    choices <- p$data$key |> unique() |> as.character()
    attr(choices, "x") <- "key"
    
    return(choices)
  })
  
  modelParamPlotCustPoints <- shinyTools::customPointsServer("modelParamPlotCustomPoints",
                                                             plot_type = "ggplot",
                                                             x_choices = x_choices)
  
  plotFun <- reactive({
    function() {
      baseplot() |> 
        shinyTools::addCustomPointsToGGplot(modelParamPlotCustPoints()) |>
        shinyTryCatch(errorTitle = "[Model Parameters]: Plotting failed")
    }
  })

  plotExportServer("exportPlot", plotFun = plotFun)

  output$plot <- renderPlot({
    plotFun()()
  })

  dataFun <- reactive({
     function() {
       if (length(model()) == 0 || any(input$modelSelection == "")) return(NULL)
       
       if((input$modelSelection %in% names(model()$models))){
         parameterValues <- extract(model()$models[[input$modelSelection]])$betaAll
         if (model()$models[[input$modelSelection]]@hasIntercept) {
           parameterNames <- c("Intercept", attr(terms(as.formula(attributes(model()$models[[input$modelSelection]])$formula)), "term.labels"))
         } else {
           parameterNames <- c(attr(terms(as.formula(attributes(model()$models[[input$modelSelection]])$formula)), "term.labels"))
         }
       } else {
         if (length(modelAVG()) == 0) return(NULL)
         
         parameterValues <- extract(modelAVG()[[input$modelSelection]])$betaAll
         if (modelAVG()[[input$modelSelection]]@hasIntercept) {
           parameterNames <- c("Intercept", attr(terms(as.formula(attributes(modelAVG()[[input$modelSelection]])$formula)), "term.labels"))
         } else {
           parameterNames <- c(attr(terms(as.formula(attributes(modelAVG()[[input$modelSelection]])$formula)), "term.labels"))
         }
       }


      exportData <-  as.data.frame(parameterValues)
      names(exportData) <- parameterNames
      exportData
     }
  })

  shinyTools::dataExportServer("exportModelParameterData", dataFun = dataFun, filename = "predictions")
}