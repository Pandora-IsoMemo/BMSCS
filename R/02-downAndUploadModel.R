#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
downloadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$h5(label),
    textAreaInput(ns("notes"), "Notes"),
    checkboxInput(ns("onlyInputs"), "Store only data and model options"),
    downloadButton(ns("downloadModel"), "Download")
  )
}


#' Server function download model
#'
#' Backend for download model module
#'
#' @param id namespace id
#' @param allParentInput (reactive) list of inputs from parent module
#' @param yEstimates (reactive) An object created by \code{\link{estimateY}}.
#'  Distributions of the dependent variable.
#' @param formulas (reactive) formulas
#' @param data (reactive) data
#' @param uploadedNotes (reactive) variable that stores content of README.txt
downloadModelServer <-
  function(id,
           data, inputs, model, uploadedNotes) {
    moduleServer(id,
                 function(input, output, session) {
                   # observe({
                   #   updateTextAreaInput(session, "notes", value = uploadedNotes())
                   # })
                   
                   output$downloadModel <- downloadHandler(
                     filename = function() {
                       paste(gsub("\ ", "_", Sys.time()), "bmsc-app.zip", sep = "_")
                     },
                     content = function(file) {
                       withProgress({
                         zipdir <- tempdir()
                         modelfile <- file.path(zipdir, "model.rds")
                         notesfile <- file.path(zipdir, "README.txt")
                         helpfile <- file.path(zipdir, "help.html")
                         
                         dataExport <- data()
                         inputExport <- reactiveValuesToList(inputs)
                         
                         if (input$onlyInputs) {
                           modelExport <- NULL
                         } else {
                           modelExport <- model()
                         }
                         
                         saveRDS(list(
                           data = dataExport,
                           inputs = inputExport,
                           model = modelExport,
                           version = paste("BMSCApp", packageVersion("BMSCApp"))
                         ),
                         file = modelfile)
                         writeLines(input$notes, notesfile)
                         save_html(getHelp(id = ""), helpfile)
                         zip::zipr(file, c(modelfile, notesfile, helpfile))
                       },
                       value = 0.8,
                       message = "Downloading ...")
                     }
                   )
                 })
  }


#' Upload model module
#'
#' UI function to upload a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
uploadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$h5(label),
    fileInput(ns("uploadModel"), label = "Load local model"),
    remoteModelsUI(ns("remoteModels")),
    tags$br(),
    tags$br()
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param id namespace id
#' @param reset (reactive) resets the selection of the online model
uploadModelServer <-
  function(id,
           reset = reactive(FALSE)) {
    moduleServer(id,
                 function(input, output, session) {
                   pathToModel <- reactiveVal(NULL)
                   
                   uploadedData <- reactiveValues(
                     inputs = NULL,
                     model = NULL,
                     version = NULL,
                     notes = NULL
                   )
                   
                   observeEvent(input$uploadModel, {
                     pathToModel(input$uploadModel$datapath)
                   })
                   
                   pathToRemote <- remoteModelsServer(
                     "remoteModels",
                     githubRepo = "bmsc-app",
                     rPackageName = "BMSCApp",
                     rPackageVersion = "BMSCApp" %>%
                       packageVersion() %>%
                       as.character(),
                     resetSelected = reset
                   )
                   
                   observeEvent(pathToRemote(), {
                     pathToModel(pathToRemote())
                   })
                   
                   observeEvent(pathToModel(), {
                     alertType <- "error"
                     
                     res <- try({
                       zip::unzip(pathToModel())
                       modelImport <- readRDS("model.rds")
                       uploadedData$notes <- readLines("README.txt")
                     })
                     
                     if (inherits(res, "try-error")) {
                       shinyalert(
                         title = "Could not load file.",
                         text = paste(
                           "The file to be uploaded should be a .zip file",
                           "that contains the following files:",
                           "help.html, model.rds, README.txt. ",
                           "If you download a model it will exactly have this format."
                         ),
                         type = alertType
                       )
                       return()
                     }
                     
                     if (!exists("modelImport") ||
                         !all(names(modelImport) %in% c("data", "inputs", "model", "version"))) {
                       shinyalert(title = "Could not load file.",
                                  text = "File format not valid for BMSC app modelling. Model object not found.",
                                  type = alertType)
                       return()
                     }
                     
                     warning <- c()
                     if (is.null(modelImport$data)) {
                       warning[["data"]] <-
                         "No input data found."
                       
                       alertType <- "warning"
                     } else {
                       warning[["data"]] <-
                         "Input data loaded. "
                       alertType <- "success"
                     }
                     
                     if (is.null(modelImport$inputs)) {
                       warning[["inputs"]] <-
                         "No model selection parameters found."
                       
                       alertType <- "warning"
                     } else {
                       warning[["inputs"]] <-
                         "Model selection parameters loaded. "
                       alertType <- "success"
                     }
                     
                     if (is.null(modelImport$model)) {
                       warning[["model"]] <- "No model results found. "
                       alertType <- "warning"
                     } else {
                       warning[["model"]] <- "Model results loaded. "
                       # no update of alertType, do not overwrite a warning
                     }
                     
                     uploadedData$data <- modelImport$data
                     uploadedData$inputs <- modelImport$inputs
                     uploadedData$model <- modelImport$model
                     
                     if (!is.null(modelImport$version)) {
                       uploadedVersion <- paste("Saved version:", modelImport$version, ".")
                     } else {
                       uploadedVersion <- ""
                     }
                     
                     dataLoadedAlert(warning, uploadedVersion, alertType)
                     
                     # clean up
                     if (file.exists("model.rds"))
                       file.remove("model.rds")
                     if (file.exists("README.txt"))
                       file.remove("README.txt")
                     if (file.exists("help.html"))
                       file.remove("help.html")
                   })
                   
                   return(uploadedData)
                 })
  }

dataLoadedAlert <-
  function(warnings,
           uploadedVersion,
           alertType) {
    shinyalert(
      title = "Upload finished",
      text = HTML(paste0(
        #"<div align='left'>",
        "<p>",
        paste(
          paste0(warnings, collapse = "<br/>"),
          uploadedVersion,
          sep = "</p><br/><p>"
        ),
        "</p>"#,
        #"</div>"
      )),
      type = alertType,
      html = TRUE
    )
  }
