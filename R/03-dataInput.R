#' @rdname shinyModule
#' @export
dataInputUI <- function(id, title = "") {
  ns <- NS(id)

  tabPanel(
    title = title,
    id = id,
    value = id,
    sidebarPanel(
      style = "position:fixed; width:31%; max-width:700px; overflow-y:auto; height:88%",
      importDataUI(ns("data"), "Import Data"),
      actionButton(ns("exampleData"), label = "Generate example data")
    ),
    mainPanel(
      fluidRow(
        DTOutput(ns("table"))
      )
    )
  )
}

#' @rdname shinyModule
#' @export
dataInput <- function(input, output, session) {
  dat <- reactiveVal(NULL)

  importedData <- importDataServer(
    "data",
    defaultSource = "file"
  )

  observeEvent(importedData(), {
    req(length(importedData()) > 0)
    dat(importedData()[[1]])
  })

  observeEvent(input$exampleData, {
    dat(generateExampleData())
  })

  output$table <- renderDT(prepareDataTable(dat()))
  
  dat
}

prepareDataTable <- function(data) {
  datatable(data)
}
