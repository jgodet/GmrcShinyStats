#' Concordance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Concordance_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Concordance Server Functions
#'
#' @noRd 
mod_Concordance_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Concordance_ui("Concordance_1")
    
## To be copied in the server
# mod_Concordance_server("Concordance_1")
