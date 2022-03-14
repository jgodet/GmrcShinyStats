#' Saisie_manuelle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Saisie_manuelle_ui <- function(id){
  ns <- NS(id)
  tagList(
 h1("Bienvenue dans l'analyse manuelle !")
  )
}
    
#' Saisie_manuelle Server Functions
#'
#' @noRd 
mod_Saisie_manuelle_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Saisie_manuelle_ui("Saisie_manuelle_1")
    
## To be copied in the server
# mod_Saisie_manuelle_server("Saisie_manuelle_1")
