
#' STARTUP
#' @author Diego
#' @return Sniny app
#' @export
#'
init_shiny_exploratorio <- function(){
  rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
  Sys.setenv("LANGUAGE"="ES")
  options(encoding = "utf8")
  shiny::runApp(appDir = system.file("application",package = "PROMIDAT.EXPLORATORIO.SHINY"), launch.browser = TRUE)
}

