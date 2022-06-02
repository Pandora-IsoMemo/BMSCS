loadData <-
  function(file,
           type,
           sep = ",",
           dec = ".",
           rownames = FALSE) {
    encTry <- as.character(guess_encoding(file)[1, 1])
    if (type == "xlsx") {
      xlsSplit <- strsplit(file, split = "\\.")[[1]]
      if (xlsSplit[length(xlsSplit)] == "xls") {
        type <- "xls"
      }
    }
    data <- switch(
      type,
      csv = suppressWarnings({
        read.csv(
          file,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry
        )
      }),
      txt = suppressWarnings({
        read.csv(
          file,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry
        )
      }),
      xlsx = read.xlsx(file),
      xls = suppressWarnings({
        readxl::read_excel(file)
      })
    )
    
    if (is.null(data))
      return(NULL)
    
    if (is.null(dim(data))) {
      stop("Could not determine dimensions of data")
      return(NULL)
    }
    
    if (any(dim(data) == 0)) {
      stop("Number of rows or columns equal to 0")
      return(NULL)
    }
    
    if (rownames) {
      rn <- data[, 1]
      data <- as.matrix(data[, -1, drop = FALSE])
      rownames(data) <- rn
      data <- as.data.frame(data)
    }
    vNames <- colnames(data)
    colnames(data) <- vNames %>% formatColumnNames()
    return(data)
  }

# ui function for import data
importDataUI <- function(id, label = "Import Data") {
  ns <- NS(id)
  actionButton(ns("openPopup"), label)
}

# server function to import data
importData <- function(input,
                       output,
                       session,
                       validateData = function(df)
                         list(),
                       rowNames = NULL,
                       colNames = NULL) {
  ns <- session$ns
  
  values <- reactiveValues(
    fileImportWarning = NULL,
    fileImportSuccess = NULL,
    dataImport = NULL,
    data = NULL
  )
  
  observeEvent(input$openPopup, ignoreNULL = TRUE, {
    reset("file")
    values$fileImportWarning <- NULL
    values$fileImportSuccess <- NULL
    values$dataImport <- NULL
    values$data <- NULL
    
    showModal(importDataDialog(ns = ns))
  })
  
  observeEvent({
    input$file
    input$type
    input$colSep
    input$decSep
    input$rownames
  }, {
    values$dataImport <- NULL
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    values$fileImportWarning <- NULL
    values$fileImportSuccess <- NULL
    
    df <- tryCatch(
      loadData(
        inFile$datapath,
        input$type,
        input$colSep,
        input$decSep,
        input$rownames
      ),
      error = function(e) {
        logError(e)
        values$fileImportWarning <- "Could not read in file."
        NULL
      },
      warning = function(w) {
        logWarning(w)
        values$fileImportWarning <- "Could not read in file."
        NULL
      }
    )
    
    if (is.null(df))
      return(NULL)
    
    ## Import technically successful
    values$dataImport <- df
    
    ## Import valid?
    val <- if (is.reactive(validateData))
      validateData()
    else
      validateData
    
    res <- try(val(df), silent = TRUE)
    
    if (inherits(res, "try-error") || !is.list(res)) {
      values$fileImportWarning <- c(values$fileImportWarning,
                                    "Could not validate file")
      return(NULL)
    }
    
    if (length(res) > 0) {
      values$fileImportWarning <- c(values$fileImportWarning,
                                    res)
      return(NULL)
    }
    values$headData <- lapply(head(as.data.frame(df)), function(z) {
      if (is.character(z)) {
        substr(z, 1, 50)
      } else {
        z
      }
    })[1:min(ncol(df), 5)]
    
    values$fileImportSuccess <- "Data import was successful"
    values$fileImportWarning <- NULL
    
  })
  
  output$warning <-
    renderText(paste(values$fileImportWarning, sep = "\n"))
  output$success <- renderText(values$fileImportSuccess)
  output$preview <- renderTable(
    values$headData,
    bordered = TRUE,
    rownames = FALSE,
    colnames = FALSE
  )
  
  observeEvent(input$cancel, {
    removeModal()
  })
  
  observeEvent(input$accept, {
    removeModal()
    
    if (length(values$fileImportWarning) == 0)
      values$data <- values$dataImport
  })
  
  return(reactive(values$data))
}

# import data dialog ui
importDataDialog <- function(ns) {
  modalDialog(
    title = "Import Data",
    footer = tagList(actionButton(ns("cancel"), "Cancel"),
                     actionButton(ns("accept"), "Accept")),
    fileInput(ns("file"), "File"),
    selectInput(
      ns("type"),
      "File type",
      choices = c("xlsx", "csv"),
      selected = "xlsx"
    ),
    conditionalPanel(
      condition = paste0("input.type == 'csv'"),
      div(style = "display: inline-block;horizontal-align:top; width: 80px;",
          textInput(
            ns("colSep"), "column separator:", value = ","
          )),
      div(style = "display: inline-block;horizontal-align:top; width: 80px;",
          textInput(
            ns("decSep"), "decimal separator:", value = "."
          )),
      ns = ns
    ),
    checkboxInput(ns("rownames"), "First column contains rownames"),
    helpText("The first row in your file need to contain variable names."),
    div(class = "text-danger", textOutput(ns("warning"))),
    div(class = "text-success", textOutput(ns("success"))),
    tableOutput(ns("preview"))
  )
}


#' Format Column Names
#'
#' Replaces all not alpha-numeric characters in the names of columns with a dot.
#'
#' @param vNames (character) names of the imported data's columns
#' @param isTest (logical) set TRUE if function is used in tests
formatColumnNames <- function(vNames, isTest = FALSE) {
  if (any(grepl("[^[:alnum:] | ^\\.]", vNames))) {
    if (!isTest) {
      shinyjs::alert("One or more column names contain non-alphanumeric characters, name changed.")
    }
    # replace non-alphanum characters with dot
    vNames <- gsub("[^[:alnum:] | ^\\.]", ".", vNames)
    # remove dots at the beginning of a column name
    vNames <- gsub("^\\.", "", vNames)
  }
  
  if (any(grepl("^[0-9]{1,}$", substr(vNames, 1, 1)))) {
    if (!isTest) {
      shinyjs::alert("One or more column names begin with number, name changed.")
    }
    # if name begins with a number paste x before name
    vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))] <-
      paste0("x", vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))])
  }
  
  return(vNames)
}
