#' @import shiny
#' @importFrom car durbinWatsonTest vif
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom DataTools downloadModelUI downloadModelServer importDataUI importDataServer
#'  remoteModelsUI remoteModelsServer tryCatchWithWarningsAndErrors uploadModelUI uploadModelServer
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom dplyr ungroup summarise group_by "%>%" summarise_ group_by_
#' @importFrom ggplot2 aes_string element_text geom_errorbar geom_point ggplot
#' labs scale_x_discrete theme aes aes_ geom_boxplot ylab xlab
#' @importFrom shinyjs useShinyjs alert
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom openxlsx write.xlsx
#' @importFrom graphics plot boxplot
#' @importFrom pROC plot.roc
#' @importFrom Rcpp evalCpp
#' @importFrom rstan extract
#' @importFrom stats lm as.formula predict rexp rnorm terms rpois median quantile sd qnorm chisq.test cor lm.fit model.matrix na.omit
#' @importFrom jsonlite toJSON
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom utils capture.output head packageVersion read.csv write.table
#' @importFrom tidyr gather
#' @importFrom BMSC constrSelEst createFormula prepModelNames get_avg_model getModelFits 
#'  get_model_weights plotModelFit
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
