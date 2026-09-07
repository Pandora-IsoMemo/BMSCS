#' Start Application
#'
#' @param port port of web application
#' @param host accept connections from this address
#' @param launch.browser whether to launch the application in a web browser
#'
#' @export
startApplication <- function(
  port = 4242,
  host = "127.0.0.1",
  launch.browser = getOption("shiny.launch.browser", interactive())
) {
  shiny::runApp(
    system.file("app", package = environmentName(topenv())),
    port = port,
    host = host,
    launch.browser = launch.browser
  )
}
