modelPredictionsTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Model Predictions",
        value = "predictionTab",
        selectInput(ns("modelSelection"), "Select Model", choices = ""),
        plotOutput(ns("plot")),
        plotExportButton(ns("exportPlot")),
        dataExportButton(ns("exportModelPredictionsData"))
    )
}

modelPredictions <- function(input, output, session, model, data, modelAVG) {
    observe({
        req(model())
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
        req(modelAVG())
        updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
    })

  plotFun <- reactive({
    function() {
      if (length(model()) == 0 ||
          !((input$modelSelection %in% names(model()$models)) ||
            (input$modelSelection %in% names(modelAVG()))))
        return(NULL)
      
      if ((input$modelSelection %in% names(model()$models))) {
        predictions <- BMSC::predict(model()$models[[input$modelSelection]], data())
      } else {
        predictions <- BMSC::predict(modelAVG()[[input$modelSelection]], data())
      }
      dependent <- data()[, model()$dependent]
      plot(dependent ~ predictions,
           ylab = model()$dependent,
           xlab = "predictions")
    }
  })

  plotExportServer("exportPlot", plotFun = plotFun)

    output$plot <- renderPlot({
        plotFun()()
    })

    dataFun <- reactive({
      function() {
        if (length(model()) == 0 || input$modelSelection == "")
          return(NULL)
        
        if ((input$modelSelection %in% names(model()$models))) {
          predictions <- BMSC::predict(model()$models[[input$modelSelection]], data())
        } else {
          if (length(modelAVG()) == 0)
            return(NULL)
          predictions <- BMSC::predict(modelAVG()[[input$modelSelection]], data())
        }
        
        dependent <- data()[, model()$dependent]
        
        data.frame(y = dependent, prediction = predictions)
      }
    })

    shinyTools::dataExportServer("exportModelPredictionsData", dataFun = dataFun, filename = "predictions")
}