textExportButton <- function(id, title = "Download") {
    ns <- NS(id)

    downloadButton(ns("download"), title)
}

textExport <- function(input, output, session, printFun, filename = "output") {
  content <- reactive({
    capture.output(printFun()())
  })

  output$text <- renderPrint({
    printFun()()
  })

  output$download <- downloadHandler(
    filename = function(){
      paste0(filename, ".txt")
    },
    content = function(file) {
      writeLines(content(), file)
    }
  )
}