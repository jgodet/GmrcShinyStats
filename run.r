
GO <- function(){
  print(getwd())
  require(shiny)
  shiny::runApp( "./R/",launch.browser = T)
  q(save="no")
}

