library("BMSCApp")

function(input, output, session) {
  require("BMSC")
  options("scipen"=100, "digits"=4)
  data <- shiny::callModule(dataInput, "data")
  shiny::callModule(modelEstimation, "model", data)
}
