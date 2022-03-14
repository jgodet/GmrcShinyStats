#' Redactions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Redactions_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Bienvenue dans la rédaction !")
  )
}
    
#' Redactions Server Functions
#'
#' @noRd 
mod_Redactions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Redactions_ui("Redactions_1")
    
## To be copied in the server
# mod_Redactions_server("Redactions_1")
