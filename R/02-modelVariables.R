modelVariablesTab <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Variable Correlations",
    value = "variablesTab",
    radioButtons(ns("modelData"), "Data variables or model variables", choices = c("Model" = "model",
                                              "Data" = "data"), selected = "data"), 
    conditionalPanel(
      condition = "input.modelData == 'model'",
      ns = ns,
      selectInput(ns("modelSelection"), "Select Model", choices = "")),
    conditionalPanel(
      condition = "input.modelData == 'data'",
      ns = ns,
      pickerInput(ns("variableSelection"), "Select Variables", choices = NULL, multiple = TRUE),
      pickerInput(ns("variableSelectionCat"), "Select categorical variables (for Cramer's V and Eta computation)", choices = NULL, multiple = TRUE)),
    h5("Table of variance inflation factors:"),
    tableOutput(ns("VIF")),
    dataExportButton(ns("exportVifs")),
    h5("Scatter plot of independent variables (select two variables):"),
    selectInput(ns("v1"), "Select variable 1", choices = NULL, multiple = FALSE, selected = NULL),
    selectInput(ns("v2"), "Select variable 2", choices = NULL, multiple = FALSE, selected = NULL),
    plotOutput(ns("plot")),
    plotExportButton(ns("exportPlot")),
    h5("Table of correlations:"),
    conditionalPanel(
      condition = "input.modelData == 'data'",
      ns = ns,
    radioButtons(ns("corType"), "Data variables or model variables",
                 choices = c("All numeric correlations" = "corr",
                             "Cramer's V (categorical with categorical)" = "cramer",
                             "Eta (numeric with categorical)" = "eta"),
                 selected = "corr"),
    ),
    verbatimTextOutput(ns("correlations")),
    dataExportButton(ns("exportCors"))
  )
}

modelVariables <- function(input, output, session, model, data, modelAVG) {
  observe({
    req(model())
    updateSelectInput(session, "modelSelection", choices = names(model()$models))
    req(modelAVG())
    updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
  })
  observe({
    req(data())
    updatePickerInput(session, "variableSelection", choices = names(data()))
  })
  observe({
    updatePickerInput(session, "variableSelectionCat", choices = input$variableSelection)
  })
  
  observe({
    if(input$modelData == "model"){
      req(model())
    if(input$modelSelection != ""){
    if((input$modelSelection %in% names(model()$models)) || (input$modelSelection %in% names(modelAVG()))){
      
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
      updateSelectInput(session, "v1", choices = c("", mPar@varNames), selected = "")
      updateSelectInput(session, "v2", choices = c("", mPar@varNames), selected = "")
    }
    }
    } else {
      req(data())
      if(!is.null(input$variableSelection) && all(input$variableSelection != "")){
          updateSelectInput(session, "v1", choices = c("", names(data())), selected = "")
          updateSelectInput(session, "v2", choices = c("", names(data())), selected = "")
      }
    }
  })
  
  
  plotFun <- reactive({
    function() {
      if (input$modelData == "model") {
        if (length(model()) == 0 ||
            is.null(input$v1) ||
            is.null(input$v2) || input$v1 == "" || input$v2 == "")
          return(NULL)
        
        if ((input$modelSelection %in% names(model()$models))) {
          mPar <- model()$models[[input$modelSelection]]
        } else {
          mPar <- modelAVG()[[input$modelSelection]]
        }
        
        vv1 <- mPar@designMatrix[, input$v1]
        vv2 <- mPar@designMatrix[, input$v2]
        if (is.numeric(vv1) && is.numeric(vv2)) {
          plot(
            vv1 ~ vv2,
            ylab = input$v1,
            xlab = input$v2,
            cex.axis = 1.5,
            cex.lab = 1.5
          )
        }
        if (is.character(vv1) && is.numeric(vv2)) {
          boxplot(
            vv2 ~ vv1,
            ylab = input$v1,
            xlab = input$v2,
            cex.axis = 1.5,
            cex.lab = 1.5
          )
        }
        if (is.character(vv2) && is.numeric(vv1)) {
          boxplot(
            vv1 ~ vv2,
            ylab = input$v1,
            xlab = input$v2,
            cex.axis = 1.5,
            cex.lab = 1.5
          )
        }
      } else {
        if (length(data()) == 0 ||
            is.null(input$v1) ||
            is.null(input$v2) || input$v1 == "" || input$v2 == "")
          return(NULL)
        
        vv1 <- data()[, input$v1]
        vv2 <- data()[, input$v2]
        if (is.numeric(vv1) && is.numeric(vv2)) {
          plot(
            vv1 ~ vv2,
            ylab = input$v1,
            xlab = input$v2,
            cex.axis = 1.5,
            cex.lab = 1.5
          )
        }
        if (is.character(vv1) && is.numeric(vv2)) {
          boxplot(
            vv2 ~ vv1,
            ylab = input$v1,
            xlab = input$v2,
            cex.axis = 1.5,
            cex.lab = 1.5
          )
        }
        if (is.character(vv2) && is.numeric(vv1)) {
          boxplot(
            vv1 ~ vv2,
            ylab = input$v1,
            xlab = input$v2,
            cex.axis = 1.5,
            cex.lab = 1.5
          )
        }
      }
    }
  })
  
  plotExportServer("exportPlot", plotFun = plotFun)
  
  output$plot <- renderPlot({
    plotFun()()
  })
  
  correlationMatrix <- reactive({
    if(input$modelData == "model"){
    req(model())
    function(){
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
      if(input$corType == "corr"){
      if(mPar@hasIntercept){
        return(cor(mPar@designMatrix[, -1, drop = FALSE]))
      } else {
        return(cor(mPar@designMatrix))
      }
      }
    }
    } else {
      req(data())
      req(!is.null(input$variableSelection))
      function(){
      if(input$corType == "cramer"){
        numericVars <- setdiff(input$variableSelection, input$variableSelectionCat)
        catVars <- intersect(input$variableSelectionCat, input$variableSelection)
        cramerMatrix <- matrix(0, ncol = length(catVars), nrow = length(catVars))
        dataTemp <- data() 
        
        for(i in 1:length(catVars)){
          for(j in 1:length(catVars)){
            fit <- chisq.test(as.matrix(dataTemp[, catVars[i], drop = FALSE]), as.matrix(dataTemp[, catVars[j], drop = FALSE]))
            cramerMatrix[i,j] <- sqrt((fit$statistic / nrow(dataTemp)) / 
                                     min(length(unique(dataTemp[, catVars[j]])), length(unique(dataTemp[, catVars[i]]))))
          }
        }
        colnames(cramerMatrix) <- catVars
        rownames(cramerMatrix) <- catVars
        return(cramerMatrix)
        }
      if(input$corType == "corr"){
          designMatrix <- model.matrix(~ . - 1, data = data()[, input$variableSelection, drop = FALSE])
          return(cor(designMatrix))
      }
      if(input$corType == "eta"){
        numericVars <- setdiff(input$variableSelection, input$variableSelectionCat)
        catVars <- intersect(input$variableSelectionCat, input$variableSelection)
        etaMatrix <- matrix(0, ncol = length(catVars), nrow = length(numericVars))
        dataTemp <- data() 
          for(i in 1:length(numericVars)){
            for(j in 1:length(catVars)){
             fit <- lm.fit(x = model.matrix(~ . - 1, data = data()[, catVars[j], drop = FALSE]), y = dataTemp[, numericVars[i]])
             etaMatrix[i,j] <- cor(fit$fitted.values, dataTemp[, numericVars[i]])
            }
          }
        colnames(etaMatrix) <- catVars
        rownames(etaMatrix) <- numericVars
        return(etaMatrix)
      }
      }
      }
    })
  
  output$correlations <- renderPrint({
    if(input$modelData == "model"){
      req(model())
      correlationMatrix()()
    } else{
      req(data())
      req(!is.null(input$variableSelection))
      correlationMatrix()()
    }
  })
  
  
  VIF <- reactive({
    if(input$modelData == "model"){
    req(model())
    function() {      
      if((input$modelSelection %in% names(model()$models))){
        mPar <- model()$models[[input$modelSelection]]
      } else {
        mPar <- modelAVG()[[input$modelSelection]]
      }
      
        designMatrix <- mPar@designMatrix
        vNames <- mPar@varNames
        if(mPar@hasIntercept){
          vNames <- vNames[-1]
          if(length(vNames) <= 1) return(data.frame(variable = vNames[length(vNames)], vif = 1))
        } else {
          if(length(vNames) <= 1) return(data.frame(variable = vNames[length(vNames)], vif = 1))
        }
        
        if(mPar@hasIntercept){
          return(data.frame(variable = vNames, vif = vif(lm(paste0("y", "~ ."),
                                  data = cbind(designMatrix, y = rnorm(nrow(designMatrix)) )[, -1]))))
        } else {
          return(data.frame(variable = vNames, vif = vif(lm(paste0("y", "~ . -1"),
                                  data = cbind(designMatrix, y = rnorm(nrow(designMatrix)) )))))
        }
    }
    } else {
      req(data())
      req(!is.null(input$variableSelection))
      function() {       
        designMatrix <- model.matrix(~ . , data = data()[, input$variableSelection, drop = FALSE])
        vNames <- colnames(designMatrix)
        if(length(vNames) <= 2) return(data.frame(variable = vNames[length(vNames)], vif = 1))
        designMatrix <- as.data.frame(designMatrix)
        designMatrix$independent <- rnorm(nrow(designMatrix))
        vifValue <- vif(lm(paste0("independent", "~ ."), data = designMatrix[, -1])) %>%
          shinyTryCatch(errorTitle = "car::vif() failed")
        if (is.null(vifValue)) vifValue <- rep(NA, length(vNames[-1]))
        return(data.frame(variable = vNames[-1], vif = vifValue))
      }
    }
  })
  
  
  output$VIF <- renderTable(VIF()(),
                            bordered = TRUE, rownames = FALSE, colnames = TRUE)
  
  shinyTools::dataExportServer("exportVifs",
                               dataFun = reactive(function() {
                                 if (length(model()) == 0 || length(data()) == 0)
                                   return(NULL)
                                 if (input$modelData == "data" &&
                                     (is.null(input$variableSelection) ||
                                      any(input$variableSelection == "")))
                                   return(NULL)
                                 
                                 VIF()()
                               }),
                               filename = "VIF")
  
  shinyTools::dataExportServer("exportCors",
                               dataFun = reactive(function() {
                                 if (length(model()) == 0 || length(data()) == 0)
                                   return(NULL)
                                 if (input$modelData == "data" &&
                                     (is.null(input$variableSelection) ||
                                      any(input$variableSelection == "")))
                                   return(NULL)
                                 
                                 correlationMatrix()() %>%
                                   as.data.frame()
                               }),
                               filename = "Corr")
  
}
