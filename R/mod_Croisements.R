#' Croisements UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Croisements_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Croisements Server Functions
#'
#' @noRd 
mod_Croisements_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Croisements_ui("Croisements_1")
    
## To be copied in the server
# mod_Croisements_server("Croisements_1")
