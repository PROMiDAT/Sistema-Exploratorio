
#' STARTUP
#' @author Diego
#' @return Sniny app
#' @export
#'
init_shiny_exploratorio <- function(){
  shiny::runApp(appDir = system.file("application",package = "PROMIDAT.EXPLORATORIO.SHINY"),launch.browser = TRUE)
}

