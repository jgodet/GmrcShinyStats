#' Tests UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Tests_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Tests Server Functions
#'
#' @noRd 
mod_Tests_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Tests_ui("Tests_1")
    
## To be copied in the server
# mod_Tests_server("Tests_1")
