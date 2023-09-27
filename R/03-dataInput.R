#' @rdname shinyModule
#' @export
dataInputUI <- function(id, title = "") {
  ns <- NS(id)

  tabPanel(
    title = title,
    id = id,
    value = id,
    sidebarPanel(
      style = "position:fixed; width:23%; max-width:500px; overflow-y:auto; height:88%",
      width = 3,
      importDataUI(ns("data"), "Import Data"),
      actionButton(ns("exampleData"), label = "Generate example data")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        DTOutput(ns("table"))
      )
    )
  )
}

#' @rdname shinyModule
#' @param config (list) list of configuration parameters
#' @export
dataInput <- function(input, output, session, config) {
  data <- reactiveVal(NULL)

  importedData <- importDataServer(
    "data",
    defaultSource = config$defaultSourceData,
    rPackageName = config$rPackageName
  )

  observeEvent(importedData(), {
    req(length(importedData()) > 0)
    data(importedData()[[1]])
  })

  observeEvent(input$exampleData, {
    data(generateExampleData())
  })

  output$table <- renderDT(prepareDataTable(data()))
  
  data
}

prepareDataTable <- function(dat) {
  datatable(dat)
}
