#' @rdname shinyModule
#' @export
dataInputUI <- function(id, title = "") {
  ns <- NS(id)

  tabPanel(
    title = title,
    id = id,
    value = id,
    sidebarPanel(
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

  importedData <- callModule(importData, "data")

  observeEvent(importedData(), {
    dat(importedData())
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
