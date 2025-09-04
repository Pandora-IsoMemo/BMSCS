#' @import shiny
#' @importFrom BMSC constrSelEst createFormula prepModelNames get_avg_model getModelFits 
#'  get_model_weights plotModelFit
#' @importFrom car durbinWatsonTest vif
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc mcmc.list
#' @importFrom DataTools downloadModelUI downloadModelServer importDataServer importDataUI
#'  importOptions importServer importUI
#' @importFrom dplyr "%>%" across bind_cols bind_rows group_by left_join mutate summarise ungroup where
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom ggplot2 aes aes_ annotate element_line element_rect element_text
#'  geom_abline geom_boxplot geom_errorbar geom_point ggplot ggplot_build labs scale_x_discrete
#'  theme theme_classic ylab xlab
#' @importFrom graphics plot boxplot
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom methods slot slotNames
#' @importFrom Rcpp evalCpp
#' @importFrom pROC auc ci.auc ggroc roc
#' @importFrom rlang .data
#' @importFrom rstan extract
#' @importFrom shinyjs useShinyjs alert
#' @importFrom shinyTools addCustomPointsToGGplot customPointsServer customPointsUI 
#'  dataExportButton dataExportServer headerButtonsUI 
#'  plotExportButton plotExportServer shinyTryCatch textExportButton textExportServer
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats as.formula chisq.test cor lm lm.fit median model.matrix na.omit 
#'  predict qnorm quantile rexp rnorm rpois sd setNames terms window
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
