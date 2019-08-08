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
  appDir <- system.file("shiny-examples", "myapp", package = "GmrcShinyStats")
  print(appDir)
  require(gmrcfun)

  if( appDir == ""){
    stop("Could not find directory. Try re-installing `GmrcShinyStats`.", call. = FALSE)
  }
  setwd(appDir)
  shiny::runApp(appDir, launch.browser = T)
  #q(save="no")
}
