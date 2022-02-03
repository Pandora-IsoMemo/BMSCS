modelSummaryTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Model Summary",
        value = "summaryTab",
        selectInput(ns("modelSelection"), "Select model", choices = ""),
        sliderInput(ns("quantileInt"), "Select quantile for credible intervals",
            min = 0.5, max = 0.999, step = 0.001, value = 0.95
        ),
        verbatimTextOutput(ns("summary")),
        textExportButton(ns("exportText"))
    )
}

modelSummary <- function(input, output, session, model) {
    observe({
        req(model())
        
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
    })

    printFun <- reactive({
        req(model())
        req((input$modelSelection %in% names(model()$models)))
        
        function() {
            print(model()$models[[input$modelSelection]], cLevel = input$quantileInt)
        }
    })

    output$summary <- renderPrint({
        req(model())
        printFun()()
    })

    callModule(textExport, "exportText", printFun = printFun, filename = "summary")
}