
#' STARTUP
#' @author Diego
#' @return Sniny app
#' @export
#'
init_shiny_exploratorio <- function(){
  .GlobalEnv$foto <- ls(envir = .GlobalEnv)
  shiny::runApp(appDir = system.file("application",package = "PROMIDAT.EXPLORATORIO.SHINY"),launch.browser = TRUE)
}

