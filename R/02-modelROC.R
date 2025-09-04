# this tab is only visible if not: m()$models[[1]]@type == "linear"
modelROCTab <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "ROC Curve",
    value = "ROC",
    selectInput(ns("modelSelection"), "Select model", choices = ""),
    tags$br(), tags$br(),
    plotOutput(ns("plot"), height = "580px"),
    tags$br(),
    fluidRow(
      column(
        4,
        textInput(ns("roctitle"), "Plot title", width = "100%", value = "ROC Curve"),
        tags$br(),
        sliderInput(
          ns("rocAxis"),
          label = "Axis label font size",
          min = 0.1,
          max = 5,
          value = 1.5,
          width = "100%"
        ),
        sliderInput(
          ns("rocAxisT"),
          label = "Axis title font size",
          min = 0.1,
          max = 5,
          value = 1.5,
          width = "100%"
        ),
        sliderInput(
          ns("rocT"),
          label = "Plot title font size",
          min = 0.1,
          max = 5,
          value = 1.5,
          width = "100%"
        ),
        checkboxInput(ns("AUC"), label = "Show AUC estimate"),
        conditionalPanel(
          condition = "input.AUC == true",
          ns = ns,
          checkboxInput(ns("AUCI"), label = "Show AUC confidence interval")
        )
      ),
      column(4, shinyTools::customPointsUI(
        id = ns("modelROCCustPoints"), plot_type = "ggplot"
      )),
      column(4, align = "right", plotExportButton(ns("exportPlot")))
    )
  )
}

modelROC <- function(input, output, session, model, data, modelAVG) {
    observe({
        req(model())
        updateSelectInput(session, "modelSelection", choices = names(model()$models))
        req(modelAVG())
        updateSelectInput(session, "modelSelection", choices = c(names(model()$models), names(modelAVG())))
    })

  modelROCPlotCustPoints <- shinyTools::customPointsServer("modelROCCustPoints",
                                                           plot_type = "ggplot")
  plotFun <- reactive({
    function() {
      if (length(model()) == 0 || is.null(input$modelSelection))
        return(NULL)
      
      p <- plot_roc_gg_app(
        modelSelection = input$modelSelection,
        AUCI = isTRUE(input$AUCI),
        AUC = isTRUE(input$AUC),
        rocAxis = input$rocAxis,
        rocAxisT = input$rocAxisT,
        roctitle = input$roctitle,
        rocT = input$rocT,
        model = model(),
        modelAVG = modelAVG(),
        dat = data()
      ) |>
        shinyTools::addCustomPointsToGGplot(modelROCPlotCustPoints()) |>
        shinyTryCatch(errorTitle = "[Model ROC]: Plotting failed")
      
      if (!is.null(p)) print(p)  # render ggplot
    }
  })

  output$plot <- renderPlot({
    plotFun()()
  }, width = 800, height = 533)
  
  plotExportServer("exportPlot", plotFun = plotFun)
}

# Plot ROC curve for logistic models using ggplot2
# 
# @param modelSelection Selected model name (string)
# @param AUCI Logical, whether to show AUC confidence interval
# @param AUC Logical, whether to show AUC value
# @param rocAxis Numeric, scaling factor for axis text size
# @param rocAxisT Numeric, scaling factor for axis title size
# @param roctitle Character, title of the ROC plot
# @param rocT Numeric, scaling factor for plot title size
# @param model Model object returned by `prepModelNames` and `getModelFits`
# @param modelAVG Averaged model object returned by `get_avg_model`
# @param dat Data frame used for model fitting
# @return ggplot object or NULL if error
plot_roc_gg_app <- function(
    modelSelection, AUCI, AUC, rocAxis, rocAxisT, roctitle, rocT,
    model, modelAVG, dat
) {
  if (is.null(modelSelection) || length(modelSelection) != 1L) {
    message("modelSelection is missing.")
    return(NULL)
  }
    
  # pick model
  mods <- model$models
  mPar <- if (modelSelection %in% names(mods)) mods[[modelSelection]] else modelAVG[[modelSelection]]
  if (is.null(mPar)) {
    message("Selected model not found.")
    return(NULL)
  }
  
  # read model type
  get_type <- function(obj) {
    t <- attr(obj, "type"); if (!is.null(t)) return(t)
    if (isS4(obj) && "type" %in% slotNames(obj)) return(slot(obj, "type"))
    NULL
  }
  if (!identical(get_type(mPar), "logistic")) {
    message("ROC curve is only available for logistic models.")
    return(NULL)
  }
  
  dep_name <- model$dependent
  
  # predictions (probabilities)
  predict_fun <- if (requireNamespace("BMSC", quietly = TRUE)) BMSC::predict
  else function(object, newdata) predict(object, newdata, type = "response")
  score <- predict_fun(mPar, newdata = dat)
  
  # ROC in percent space
  roc_obj <- roc(response = dat[[dep_name]], predictor = score, percent = TRUE)
  
  # sizes mapped from cex
  sz_axis_text  <- 11 * rocAxis
  sz_axis_title <- 12 * rocAxisT
  sz_title      <- 14 * rocT
  
  # base plot
  x_mid <- 50; y_mid <- 100 - x_mid; x_off <- 0; y_off <- -2
  
  p <- ggroc(roc_obj, size = 1) +
    geom_abline(slope = 1, intercept = 100, linetype = "dashed", color = "grey50") +
    labs(title = roctitle, x = "Specificity (%)", y = "Sensitivity (%)") +
    theme_classic() +
    theme(
      axis.text  = element_text(size = sz_axis_text),
      axis.title = element_text(size = sz_axis_title),
      plot.title = element_text(size = sz_title, hjust = 0.5)
    )
  
  # Only add AUC text if AUC == TRUE (like print.auc in base R)
  if (isTRUE(AUC)) {
    auc_val <- as.numeric(auc(roc_obj))
    if (isTRUE(AUCI)) {
      ci_auc <- ci.auc(roc_obj)
      auc_lab <- sprintf("AUC: %.1f%% (%.1f%%–%.1f%%)", auc_val, ci_auc[1], ci_auc[3])
    } else {
      auc_lab <- sprintf("AUC: %.1f%%", auc_val)
    }
    p <- p + annotate("text",
                      x = x_mid + x_off, y = y_mid + y_off,
                      label = auc_lab, hjust = 0, vjust = 0)
  }
  
  p
}
