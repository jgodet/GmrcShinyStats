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
  print(getwd())
  require(shiny)
  shiny::runApp(GmrcShinyStats::app(),launch.browser = T)
  #q(save="no")
}
