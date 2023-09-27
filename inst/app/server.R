library("BMSCApp")
library(yaml)

options(shiny.maxRequestSize = 100*1024^2)

# load config variables
configFile <- system.file("config.yaml", package = "BMSCApp")
appConfig <- yaml::read_yaml(configFile)

function(input, output, session) {
  require("BMSC")
  options("scipen"=100, "digits"=4)
  data <- shiny::callModule(dataInput, "data", config = appConfig)
  shiny::callModule(modelEstimation, "model", data, config = appConfig)
  
  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })
}
