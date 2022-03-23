#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @import dashboardthemes
app_server <- function(input, output, session) {
  # Your application server logic
  r<-reactiveValues(
  )
  
  
  
  mod_chargement_server("chargement_1",r)
  mod_Croisements_server("Croisements_1",r)
  mod_Survie_server("Survie_1",r)
  mod_Tests_server("Tests_1",r)
  mod_Concordance_server("Concordance_1",r)
  mod_Accueil_server("Accueil_1")
  mod_Descriptifs_server("Descriptifs_1",r)
  
  mod_SaisieManuelle_server("SaisieManuelle_1")
  
  #callModule(mod_Descriptifs_server,id = "select",session = session, r = r)
}
