modelParametersTab <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Model Parameters",
    value = "parameterTab",
    selectInput(ns("modelSelection"), "Select Model", choices = ""),
    plotOutput(ns("plot")),
    plotExportButton(ns("exportPlot")),
    dataExportButton(ns("exportData"))
  )
}

modelParameters <- function(input, output, session, model) {
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
  })

  plotFun <- reactive({
    req(model())
    req((input$modelSelection %in% names(model()$models)))
    function() {
      mPar <- model()$models[[input$modelSelection]]
      parameterValues <- extract(mPar)$betaAll
      
      if (mPar@hasIntercept) {
        parameterValues[, 1] <-  mPar@scaleYCenter + 
          mean(mPar@scaleYScale) * (parameterValues[, 1] - 
                                       rowSums(sweep(parameterValues[, - 1, drop = FALSE], 2,
                                                     (mPar@scaleCenter / mPar@scaleScale), '*')))
        parameterValues[, -1] <- sweep(parameterValues[, - 1, drop = FALSE], 2,
                                  (mean(mPar@scaleYScale) / mPar@scaleScale), '*')
      } else {
        parameterValues <- sweep(parameterValues, 2, (mean(mPar@scaleYScale) / mPar@scaleScale), '*')
      }
      
        parameterNames <- mPar@varNames

        parameterValues <- as.data.frame(parameterValues)
        names(parameterValues) <- parameterNames
        parameterValues <- gather(parameterValues)
        dataSummary <- parameterValues %>%
          group_by_(~key) %>%
          summarise_(
            # sd = sd(estimate),
            med =  ~ median(value),
            meanEst = ~ mean(value),
            q68 = ~ quantile(value, 0.68),
            q95 = ~ quantile(value, 1 - ((1 - 0.95) / 2)),
            q32 = ~ quantile(value, 1- 0.68),
            q05 = ~ quantile(value, (1 - 0.95) / 2)
          ) %>%
          ungroup
        p <- ggplot(dataSummary, aes_(x = ~ key)) +
          ylab("Estimate") + xlab("")
        
        p <- p + geom_boxplot(mapping = aes_(
          lower = ~q32,
          upper = ~q68,
          middle = ~med,
          ymin = ~q05,
          ymax = ~q95
        ),
        stat = "identity"
        ) + geom_errorbar(aes_(ymin = ~meanEst, ymax = ~meanEst), linetype = "dashed", data = dataSummary)
        p
        # boxplot(
        #   parameterValues,
        #   names = parameterNames,
        #   cex.axis = 1.5, cex.lab = 1.5
        # )
    }
  })

  callModule(plotExport, "exportPlot", plotFun = plotFun)

  output$plot <- renderPlot({
    plotFun()()

  })

  dataFun <- reactive({

     function() {
      parameterValues <- extract(model()$models[[input$modelSelection]])$betaAll

      if (model()$models[[input$modelSelection]]@hasIntercept) {
        parameterNames <- c("Intercept", attr(terms(as.formula(attributes(model()$models[[input$modelSelection]])$formula)), "term.labels"))
      } else {
        parameterNames <- c(attr(terms(as.formula(attributes(model()$models[[input$modelSelection]])$formula)), "term.labels"))
      }

      exportData <-  as.data.frame(parameterValues)
      names(exportData) <- parameterNames
      exportData
     }
  })

  callModule(dataExport, "exportData", data = dataFun, filename = "predictions")
}