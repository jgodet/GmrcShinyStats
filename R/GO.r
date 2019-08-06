# GO.r
# written by JuG
# August 06 2019


#' Go to GMRC shiny Stats
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#' GO()
#'
#' @return
#' @export

GO <- function(){
  appDir <- system.file("shinyStuff", "myapp", package = "GmrcShinyStats")
  if( appDir == ""){
    stop("Could not find directory. Try re-installing `GmrcShinyStats`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
  #q(save="no")
}
