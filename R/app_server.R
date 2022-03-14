#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  mod_Avec_une_base_de_donnees_server("Avec_une_base_de_donnees_1")
  mod_Saisie_manuelle_server("Saisie_manuelle_1")
  mod_Redactions_server("Redactions_1")
  mod_Croisements_server("Croisements_1")
  mod_Survie_server("Survie_1")
  mod_Tests_server("Tests_1")
  mod_Concordance_server("Concordance_1")
  mod_Base_de_donnees_server("Base_de_donnees_1")
  mod_Accueil_server("Accueil_1")
  mod_Descriptifs_server("Descriptifs_1")
}
