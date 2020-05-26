
GO <- function(){
  print(getwd())
  require(shinyFilesButton)
  require(shiny)
  shiny::runApp( "./R/",launch.browser = T)
  q(save="no")
}

