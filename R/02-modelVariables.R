modelVariablesTab <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Variable Correlations",
    value = "variablesTab",
    tags$br(),
    fluidRow(column(
      4,
      radioButtons(
        ns("modelData"),
        "Data variables or model variables",
        choices = c("Model" = "model", "Data" = "data"),
        selected = "data"
      )
    ),
    column(
      8,
      conditionalPanel(
        condition = "input.modelData == 'model'",
        ns = ns,
        selectInput(ns("modelSelection"), "Select model", choices = "")
      ),
      conditionalPanel(condition = "input.modelData == 'data'",
                       ns = ns,
                       fluidRow(column(
                         6,
                         pickerInput(
                           ns("variableSelection"),
                           "Select Variables",
                           choices = NULL,
                           multiple = TRUE,
                           width = "100%"
                         )
                       ), column(
                         6,
                         pickerInput(
                           ns("variableSelectionCat"),
                           "Select categorical variables (for Cramer's V and Eta computation below)",
                           choices = NULL,
                           multiple = TRUE,
                           width = "100%"
                         )
                       )))
    )), 
    h5("Table of variance inflation factors:"),
    tableOutput(ns("VIF")),
    dataExportButton(ns("exportVifs")),
    tags$hr(),
    fluidRow(
      column(
        8,
        h5("Scatter plot of independent variables (select two variables):"),
        selectInput(ns("v1"), "Select variable 1", choices = NULL, multiple = FALSE, selected = NULL),
        selectInput(ns("v2"), "Select variable 2", choices = NULL, multiple = FALSE, selected = NULL),
        tags$br(),
        plotOutput(ns("plot")),
        plotExportButton(ns("exportPlot")),
      ),
      column(4, shinyTools::customPointsUI(
        id = ns("modelVariablesCustPoints"),
        plot_type = "ggplot"
      ))
    ),
    tags$hr(),
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
    dataExportButton(ns("exportCors")),
    tags$br()
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
  
  modelVariablesPlotCustPoints <- shinyTools::customPointsServer("modelVariablesCustPoints",
                                                                 plot_type = "ggplot")
  
  plotFun <- reactive({
    function() {
      if (is.null(input$modelData) || input$modelData == "")
        return(NULL)
      
      # capture everything for reactivity
      if (length(model())) mod <- model() else mod <- NULL
      if (length(modelAVG())) mav <- modelAVG() else mav <- NULL
      if (length(data())) d <- data() else d <- NULL
      if (identical(input$modelData, "model"))
        src <- "model"
      else
        src <- "data"
      
      p <- plot_vars_gg(
        source = src,
        v1 = input$v1,
        v2 = input$v2,
        model = mod,
        modelAVG = mav,
        modelSelection = input$modelSelection,
        dat = d,
        cex_axis = 1.5,
        cex_lab = 1.5,
        cex_title = 1.5
      ) |>
        shinyTools::addCustomPointsToGGplot(modelVariablesPlotCustPoints()) |>
        shinyTryCatch(errorTitle = "[Variable Correlation]: Plotting failed")
      if (!is.null(p)) print(p)
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

# Helpers ----

# Coerce a vector into the right "type bucket"
is_num <- function(x) is.numeric(x)
is_cat <- function(x) is.character(x) || is.factor(x)

# Access design matrix from various model shapes
get_design_matrix <- function(mPar) {
  if (isS4(mPar) && "designMatrix" %in% slotNames(mPar)) {
    return(slot(mPar, "designMatrix"))
  }
  if (!is.null(mPar$designMatrix)) return(mPar$designMatrix)
  dm <- attr(mPar, "designMatrix", exact = TRUE)
  if (!is.null(dm)) return(dm)
  NULL
}

# Fetch v1/v2 from either the model design matrix or the raw data
fetch_vv <- function(source, v1, v2, model, modelAVG, modelSelection, dat) {
  if (source == "model") {
    mods <- model$models
    mPar <- if (modelSelection %in% names(mods)) mods[[modelSelection]] else modelAVG[[modelSelection]]
    if (is.null(mPar)) {
      message("Selected model not found.")
      return(NULL)
    }
    DM <- get_design_matrix(mPar)
    if (is.null(DM)) {
      message("No designMatrix found in model.")
      return(NULL)
    }
    list(vv1 = DM[[v1]], vv2 = DM[[v2]], xlab = v2, ylab = v1)
  } else {
    if (!is.data.frame(dat)) {
      message("'dat' must be a data.frame.")
      return(NULL)
    }
    list(vv1 = dat[[v1]], vv2 = dat[[v2]], xlab = v2, ylab = v1)
  }
}

# Map "cex-like" knobs to ggplot sizes
sizes_from_cex <- function(cex_axis = 1.5, cex_lab = 1.5, cex_title = 1.1) {
  list(
    axis_text  = 11 * cex_axis,
    axis_title = 12 * cex_lab,
    title      = 14 * cex_title
  )
}

# Plotting function using ggplot2 ----

# Plot two variables (numeric-numeric scatter or categorical-numeric boxplot)
# source = "model" uses the selected model's designMatrix; "data" uses dat
plot_vars_gg <- function(
    source = c("model","data"),
    v1, v2,
    model = NULL, modelAVG = NULL, modelSelection = NULL,
    dat = NULL,
    cex_axis = 1.5, cex_lab = 1.5, cex_title = 1.5
) {
  source <- match.arg(source)
  
  # quick input checks
  if (is.null(v1) || is.null(v2) || v1 == "" || v2 == "") {
    message("v1/v2 missing.")
    return(NULL)
  }
  
  vv <- fetch_vv(source, v1, v2, model, modelAVG, modelSelection, dat)
  if (is.null(vv)) return(NULL)
  
  vv1 <- vv$vv1; vv2 <- vv$vv2
  sz  <- sizes_from_cex(cex_axis, cex_lab, cex_title)
  
  # Cases:
  # - numeric vs numeric  -> scatter (vv1 ~ vv2)
  # - categorical vs numeric -> boxplot (vv2 ~ vv1)
  # - numeric vs categorical -> boxplot (vv1 ~ vv2)
  # - categorical vs categorical -> not supported (return NULL)
  
  if (is_num(vv1) && is_num(vv2)) {
    df <- data.frame(x = vv2, y = vv1)
    p <- ggplot(df, aes(x = .data$x, y = .data$y)) +
      geom_point(shape = 1, size = 2) +
      labs(x = vv$xlab, y = vv$ylab) +
      theme_classic() +
      theme(
        axis.text  = element_text(size = sz$axis_text),
        axis.title = element_text(size = sz$axis_title),
        plot.title = element_text(size = sz$title, hjust = 0.5)
      )
    return(p)
  }
  
  if (is_cat(vv1) && is_num(vv2)) {
    df <- data.frame(g = as.factor(vv1), y = vv2)
    p <- ggplot(df, aes(x = .data$g, y = .data$y)) +
      geom_boxplot(outlier.shape = 1, fill = "grey80", colour = "black") +
      theme_classic() +
      labs(x = v1, y = v2) +  # x shows the categorical (v1), y is numeric (v2)
      theme(
        axis.text  = element_text(size = sz$axis_text),
        axis.title = element_text(size = sz$axis_title),
        plot.title = element_text(size = sz$title, hjust = 0.5)
      )
    return(p)
  }
  
  if (is_cat(vv2) && is_num(vv1)) {
    df <- data.frame(g = as.factor(vv2), y = vv1)
    p <- ggplot(df, aes(x = .data$g, y = .data$y)) +
      geom_boxplot(outlier.shape = 1, fill = "grey80", colour = "black") +
      theme_classic() +
      labs(x = v2, y = v1) +  # x shows the categorical (v2), y is numeric (v1)
      theme(
        axis.text  = element_text(size = sz$axis_text),
        axis.title = element_text(size = sz$axis_title),
        plot.title = element_text(size = sz$title, hjust = 0.5)
      )
    return(p)
  }
  
  message("Both variables are categorical; no plot produced.")
  NULL
}

