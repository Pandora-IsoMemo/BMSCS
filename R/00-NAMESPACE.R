#' @import shiny
#' @importFrom BMSC constrSelEst createFormula prepModelNames get_avg_model getModelFits 
#'  get_model_weights plotModelFit
#' @importFrom car durbinWatsonTest vif
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom DataTools downloadModelUI downloadModelServer importDataUI importDataServer
#'  tryCatchWithWarningsAndErrors
#' @importFrom dplyr bind_rows ungroup summarise group_by "%>%" summarise_ group_by_
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom ggplot2 aes_string element_text geom_errorbar geom_point ggplot
#'  labs scale_x_discrete theme aes aes_ geom_boxplot ylab xlab
#' @importFrom graphics plot boxplot
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom jsonlite toJSON
#' @importFrom openxlsx write.xlsx
#' @importFrom Rcpp evalCpp
#' @importFrom pROC plot.roc
#' @importFrom rstan extract
#' @importFrom shinyjs useShinyjs alert
#' @importFrom shinyTools dataExportButton dataExportServer
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats lm as.formula predict rexp rnorm terms rpois median quantile sd qnorm 
#'  chisq.test cor lm.fit model.matrix na.omit
#' @importFrom tidyr gather
#' @importFrom utils capture.output head packageVersion read.csv write.table
#' @importFrom yaml read_yaml
NULL

#' Server and UI Functions for Shiny Module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param id namespace id
#' @param title title of tab in tabset panel
#' @param data data reactive
#'
#' @name shinyModule
NULL
