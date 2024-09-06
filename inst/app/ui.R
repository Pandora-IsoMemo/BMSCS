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
  div(
    id = "header-right",
    div(
      id = "logo-mpi",
      tags$a(href = "https://www.mpg.de/en",
             img(src = "MPIlogo.png", alt = "Supported by the Max Planck society"),
             target = "_blank"
      )
    ),
    div(
      id = "logo-isomemo",
      tags$a(href = "https://isomemo.com/",
             img(src = "IsoMemoLogo.png", alt = "IsoMemo"),
             target = "_blank"
      )
    ),
    div(
      id = "help",
      actionButton("getHelp", "Help")
    )
  )
)
