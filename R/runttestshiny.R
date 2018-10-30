#' Start ttestshiny
#' @title Launch ttestshiny Interface
#' @return Nothing
#' @description ttestshiny() loads interactive user interface built using R shiny.
#' @details The interactive user interface is to provide an easy way for people who are learning ttest. Includes example data for testing out a few example analysis.
#' @keywords ttestshiny,runttestshiny
#' @examples
#' \dontrun{
#' library(shiny)
#' ttestshiny()
#' }

runttestshiny <- function() {


  shiny::runApp(appDir = system.file("shiny-examples", "myapp", package = "ttestshiny"))
  Sys.setenv("R_TESTS" = "")
}
