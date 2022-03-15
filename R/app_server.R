#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, header = input$header)
  })

  mod_chargement_server("chargement_1")
  mod_Croisements_server("Croisements_1")
  mod_Survie_server("Survie_1")
  mod_Tests_server("Tests_1")
  mod_Concordance_server("Concordance_1")
  mod_Accueil_server("Accueil_1")
  mod_Descriptifs_server("Descriptifs_1")
}
