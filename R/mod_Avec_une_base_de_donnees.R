#' Avec_une_base_de_donnees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Avec_une_base_de_donnees_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    h1("Bienvenue dans l'analyse avec une base de données !"),
    
    box(plotOutput("plot1", height = 250)),
    
    box(
      title = "Controls",
      sliderInput("slider", "Number of observations:", 1, 100, 50)
    )
  )

}
    
#' Avec_une_base_de_donnees Server Functions
#'
#' @noRd 
mod_Avec_une_base_de_donnees_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Avec_une_base_de_donnees_ui("Avec_une_base_de_donnees_1")
    
## To be copied in the server
# mod_Avec_une_base_de_donnees_server("Avec_une_base_de_donnees_1")
