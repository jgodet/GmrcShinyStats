# app.r
# written by JuG
# August 01 2019


#' Do something
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'
#'
#' @return
#' @export



los <- function(){

 library(shiny)
  if(!require('shiny')){install.packages('shiny')}
  require(shiny)
 shinyApp(ui, server)
}

