modelDWTab <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Durbin-Watson Test",
    value = "DW",
    selectInput(ns("t"), "Time variable (optional)", choices = NULL, multiple = FALSE, selected = NULL),
    selectInput(ns("modelSelection"), "Select Model", choices = ""),
    verbatimTextOutput(ns("DWsummary")),
    textExportButton(ns("exportText")),
    sliderInput(ns("lagDW"), "Maximum lag", min = 1, max = 12, step = 1, value = 1)
  )
}

modelDW <- function(input, output, session, model, data, modelAVG) {
  observe({
    updateSelectInput(session, "t", choices = c("", names(data())), selected = "")
  })

  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
    req(modelAVG())
    updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
  })

  tVar <- reactive({
    if (!is.null(input$t) & input$t != "") {
      return(data()[, input$t])
    } else {
      return(NULL)
    }
  })

  printFun <- reactive({
    req(model())
    req((input$modelSelection %in% names(model()$models)) || (input$modelSelection %in% names(modelAVG())))
    
    function() {
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
      if (!is.null(tVar())) {
        print(durbinWatsonTest(lm(I(data()[, model()$dependent] -
          BMSC::predict(mPar, newdata = data())) ~
        tVar()), max.lag = input$lagDW))
      } else {
        print(durbinWatsonTest(lm(I(data()[, model()$dependent] -
          BMSC::predict(mPar, newdata = data())) ~
        I(1:length(data()[, model()$dependent]))), max.lag = input$lagDW))
      }
    }
  })

  output$DWsummary <- renderPrint({
    req(model())

    printFun()()
  })

  callModule(textExport, "exportText", printFun = printFun, filename = "summary")
}