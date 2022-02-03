plotExport <- function(input, output, session, plotFun, type = "plot"){
  observeEvent(input$export, {
    exportTypeChoices <- c("png", "pdf", "svg", "tiff")

    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      plotOutput(session$ns("plot"), height = "300px"),
      selectInput(
        session$ns("exportType"), "Filetype",
        choices = exportTypeChoices
      ),
      numericInput(session$ns("width"), "Width (px)", value = 1280),
      numericInput(session$ns("height"), "Height (px)", value = 800),
      downloadButton(session$ns("exportExecute"), "Export"),
      easyClose = TRUE
    ))
  })

  output$plot <- renderPlot({
    plotFun()()
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      paste0(gsub("-", "", Sys.Date()), "_", type, ".", input$exportType)
    },
    content = function(file){
        switch(
          input$exportType,
          png = png(file, width = input$width, height = input$height),
          pdf = pdf(file, width = input$width / 72, height = input$height / 72),
          tiff = tiff(file, width = input$width, height = input$height),
          svg = svg(file, width = input$width / 72, height = input$height / 72)
        )
        print(plotFun()())
        dev.off()
    }
  )
}

plotExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("export"), "Export Plot")
}