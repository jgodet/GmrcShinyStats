
GO <- function(){
  print(getwd())
  require(shiny)
  require(shinyFilesButton)
  shiny::runApp( "./R/",launch.browser = T)
  q(save="no")
}

