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

modelPredictions <- function(input, output, session, model, data, modelAVG) {
    observe({
        req(model())
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
        req(modelAVG())
        updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
    })

    plotFun <- reactive({
        req(model())
        req((input$modelSelection %in% names(model()$models)) || (input$modelSelection %in% names(modelAVG())))
        
        function() {
            if((input$modelSelection %in% names(model()$models))){
                predictions <- BMSC::predict(model()$models[[input$modelSelection]], data())
            } else {
                predictions <- BMSC::predict(modelAVG()[[input$modelSelection]], data())
            }
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
        req((input$modelSelection %in% names(model()$models)) || (input$modelSelection %in% names(modelAVG())))
        
        function() {            
            if((input$modelSelection %in% names(model()$models))){
                predictions <- BMSC::predict(model()$models[[input$modelSelection]], data())
            } else {
                predictions <- BMSC::predict(modelAVG()[[input$modelSelection]], data())
            }
            
            dependent <- data()[, model()$dependent]
            
            data.frame(y = dependent, prediction = predictions)
        }
    })

    callModule(dataExport, "exportData", data = dataFun, filename = "predictions")
}