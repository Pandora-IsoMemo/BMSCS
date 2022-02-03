modelPredictionsTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Model Predictions",
        value = "predictionTab",
        selectInput(ns("modelSelection"), "Select Model", choices = ""),
        plotOutput(ns("plot")),
        plotExportButton(ns("exportPlot")),
        dataExportButton(ns("exportData"))
    )
}

modelPredictions <- function(input, output, session, model, data) {
    observe({
        req(model())
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
    })

    plotFun <- reactive({
        req(model())
        req((input$modelSelection %in% names(model()$models)))
        
        function() {
            predictions <- BMSC::predict(model()$models[[input$modelSelection]], data())
            dependent <- data()[, model()$dependent]

            plot(dependent ~ predictions, ylab = model()$dependent, xlab = "predictions")
            # for export
            # 
        }
    })

    callModule(plotExport, "exportPlot", plotFun = plotFun)

    output$plot <- renderPlot({
        plotFun()()
    })

    dataFun <- reactive({
        req(model())
        req((input$modelSelection %in% names(model()$models)))
        
        function() {            
            predictions <- BMSC::predict(model()$models[[input$modelSelection]], data())
            dependent <- data()[, model()$dependent]
            
            data.frame(y = dependent, prediction = predictions)
        }
    })

    callModule(dataExport, "exportData", data = dataFun, filename = "predictions")
}