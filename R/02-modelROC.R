modelROCTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "ROC Curve",
        value = "ROC",
        selectInput(ns("modelSelection"), "Select Model", choices = ""),
        textInput(ns("roctitle"), "Plot title"),
        sliderInput(ns("rocAxis"), label = "Axis label font size", min = 0.1, max = 5, value = 1.5),
        sliderInput(ns("rocAxisT"), label = "Axis title font size", min = 0.1, max = 5, value = 1.5),
        sliderInput(ns("rocT"), label = "Plot title font size", min = 0.1, max = 5, value = 1.5),
        checkboxInput(ns("AUC"), label = "Show AUC estimate"),
        conditionalPanel(
            condition = "input.AUC == true",
            ns = ns,
        checkboxInput(ns("AUCI"), label = "Show AUC confidence interval")
        ),
        plotOutput(ns("plot")),
        plotExportButton(ns("exportPlot"))
    )
}

modelROC <- function(input, output, session, model, data) {
    observe({
        req(model())
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
    })

    plotFun <- reactive({
        req(model())
        req((input$modelSelection %in% names(model()$models)))
        
        function() {
            if(model()$models[[input$modelSelection]]@type == "logistic"){
                plot.roc(data()[, model()$dependent], BMSC::predict(model()$models[[input$modelSelection]], newdata = data()),
                        percent=TRUE,
                        ci=input$AUCI, print.auc=input$AUC, cex.axis = input$rocAxis, cex.lab = input$rocAxisT, main = input$roctitle, cex.main = input$rocT)
            }
        }
    })

    callModule(plotExport, "exportPlot", plotFun = plotFun)

    output$plot <- renderPlot({
        plotFun()()
    }, width = 800, height = 533)
}