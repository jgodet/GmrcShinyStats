#' Survie UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Survie_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Survie Server Functions
#'
#' @noRd 
mod_Survie_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Survie_ui("Survie_1")
    
## To be copied in the server
# mod_Survie_server("Survie_1")
