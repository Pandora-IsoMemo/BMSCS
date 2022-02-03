library("BMSCApp")

shiny::tagList(
  shiny::navbarPage(
    title = paste("BMSC App", utils::packageVersion("BMSCApp")),
    theme = shinythemes::shinytheme("flatly"),
    id = "tabs",
    dataInputUI("data", "Data Input"),
    modelEstimationUI("model", "Model Input")
  )
)
