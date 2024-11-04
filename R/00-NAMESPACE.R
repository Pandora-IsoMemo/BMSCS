#' @import shiny
#' @importFrom BMSC constrSelEst createFormula prepModelNames get_avg_model getModelFits 
#'  get_model_weights plotModelFit
#' @importFrom car durbinWatsonTest vif
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom DataTools downloadModelUI downloadModelServer importDataServer importDataUI
#'  importOptions importServer importUI
#' @importFrom dplyr "%>%" bind_cols bind_rows group_by left_join summarise ungroup
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom ggplot2 aes_string element_text geom_errorbar geom_point ggplot
#'  labs scale_x_discrete theme aes aes_ geom_boxplot ylab xlab
#' @importFrom graphics plot boxplot
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom Rcpp evalCpp
#' @importFrom pROC plot.roc
#' @importFrom rlang .data
#' @importFrom rstan extract
#' @importFrom shinyjs useShinyjs alert
#' @importFrom shinyTools dataExportButton dataExportServer plotExportButton plotExportServer
#'  shinyTryCatch textExportButton textExportServer
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats as.formula chisq.test cor lm lm.fit median model.matrix na.omit 
#'  predict qnorm quantile rexp rnorm rpois sd setNames terms
#' @importFrom tidyr gather
#' @importFrom utils capture.output head packageVersion
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
