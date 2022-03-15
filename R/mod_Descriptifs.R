#' Descriptifs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Descriptifs_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Descriptifs Server Functions
#'
#' @noRd 
mod_Descriptifs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Descriptifs_ui("Descriptifs_1")
    
## To be copied in the server
# mod_Descriptifs_server("Descriptifs_1")
