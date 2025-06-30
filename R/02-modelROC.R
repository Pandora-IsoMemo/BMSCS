modelROCTab <- function(id) {
    ns <- NS(id)

    tabPanel(
        "ROC Curve",
        value = "ROC",
        selectInput(ns("modelSelection"), "Select model", choices = ""),
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

modelROC <- function(input, output, session, model, data, modelAVG) {
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
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
      if (model()$models[[input$modelSelection]]@type == "logistic") {
        plot.roc(
          data()[, model()$dependent],
          BMSC::predict(mPar, newdata = data()),
          percent = TRUE,
          ci = input$AUCI,
          print.auc = input$AUC,
          cex.axis = input$rocAxis,
          cex.lab = input$rocAxisT,
          main = input$roctitle,
          cex.main = input$rocT
        )
      }
    }
  })

  plotExportServer("exportPlot", plotFun = plotFun)

    output$plot <- renderPlot({
        plotFun()()
    }, width = 800, height = 533)
}