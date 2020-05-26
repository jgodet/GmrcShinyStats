
GO <- function(){
  print(getwd())
  require(shinyFiles)
  require(shiny)
  shiny::runApp( "./R/",launch.browser = T)
  q(save="no")
}

