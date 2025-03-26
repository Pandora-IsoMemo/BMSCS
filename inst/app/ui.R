library("BMSCS")

shiny::tagList(
  shinyjs::useShinyjs(),
  shiny::navbarPage(
    includeCSS("www/custom.css"),
    title = paste("BMSC App", utils::packageVersion("BMSCS")),
    theme = shinythemes::shinytheme("flatly"),
    id = "tabs",
    dataInputUI("data", "Data Input"),
    modelEstimationUI("model", "Model Input")
  ),
  shinyTools::headerButtonsUI(id = "header", help_link = "https://pandora-isomemo.github.io/BMSCS/")
)
