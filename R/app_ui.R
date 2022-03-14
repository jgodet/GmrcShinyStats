#' ui
#' 
#' @param request needed for bookmarking
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyFiles
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = "Avec une base de données"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Accueil", tabName = "accueil", icon = icon("dashboard")),
          menuItem("Base de données", tabName = "base", icon = icon("th")),
          menuItem("Descriptif", tabName = "descriptif", icon = icon("th")),
          menuItem("Croisements/Inférence", tabName = "croisements", icon = icon("th")),
          menuItem("Analyse de survie", tabName = "survie", icon = icon("th")),
          menuItem("Tests diagnostiques", tabName = "tests", icon = icon("th")),
          menuItem("Concordance", tabName = "concordance", icon = icon("th"))
        )
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "accueil",
                  mod_Accueil_ui("Accueil_1")
          ),
          
          # Second tab content
          tabItem(tabName = "base",
                  mod_Base_de_donnees_ui("Base_de_donnees_1")
          ),
          
          #Third tab content
          tabItem(tabName = "descriptif",
                  mod_Descriptifs_ui("Descriptifs_1")
          ),
          tabItem(tabName = "croisements",
                  mod_Croisements_ui("Croisements_1")
          ),
          tabItem(tabName = "survie",
                  mod_Survie_ui("Survie_1")
          ),
          tabItem(tabName = "tests",
                  mod_Tests_ui("Tests_1")
          ),
          tabItem(tabName = "concordance",
                  mod_Concordance_ui("Concordance_1")
          )
        )
      )
    ),
    dashboardPage(
      dashboardHeader(title = "Saisie manuelle"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Avec une base de données", tabName = "dashboard", icon = icon("dashboard"))
        )
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "dashboard",
                  mod_Avec_une_base_de_donnees_ui("Avec_une_base_de_donnees_1")
          )
        )
      ),
      
    ),
    dashboardPage(
      dashboardHeader(title = "Rédaction"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Avec une base de données", tabName = "dashboard", icon = icon("dashboard"))
        )
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "dashboard",
                  mod_Avec_une_base_de_donnees_ui("Avec_une_base_de_donnees_1")
          )
        )
      ),
      
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = "golemdashboard")
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$title("golemdashboard")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    
  )
}
