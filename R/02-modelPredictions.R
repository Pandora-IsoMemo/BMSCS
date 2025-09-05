modelPredictionsTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Model Predictions",
        value = "predictionTab",
        selectInput(ns("modelSelection"), "Select model", choices = ""),
        tags$br(), tags$br(), 
        plotOutput(ns("plot")), 
        tags$br(), 
        fluidRow(
          column(6, shinyTools::customPointsUI(
            id = ns("modelPredictionsCustPoints"),
            plot_type = "ggplot"
          )),
          column(
            6,
            align = "right",
            plotExportButton(ns("exportPlot")),
            shinyTools::dataExportButton(ns("exportModelPredictionsData"))
          )
        )
    )
}

modelPredictions <- function(input, output, session, model, data, modelAVG) {
    observe({
        req(model())
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
        req(modelAVG())
        updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
    })
  
  modelPredictionsPlotCustPoints <- shinyTools::customPointsServer("modelPredictionsCustPoints",
                                                                   plot_type = "ggplot")

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
      # plot(dependent ~ predictions,
      #      ylab = model()$dependent,
      #      xlab = "predictions")
      
      # replacing plot() with ggplot2 to add custom points
      ggplot(data.frame(predictions, dependent),
                  aes(x = predictions, y = dependent)) +
        geom_point(shape = 1,
                   color = "black",
                   size = 2) +
        labs(title = " ",
             x = "predictions",
             y = model()$dependent) +
        theme_classic(base_size = 14) +
        theme(
          panel.border = element_rect(fill = NA, color = "black"),
          axis.line = element_line(color = "black")
        ) |>
        shinyTools::addCustomPointsToGGplot(modelPredictionsPlotCustPoints()) |>
        shinyTryCatch(errorTitle = "[Model Predictions]: Plotting failed")
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