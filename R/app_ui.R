#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    tabsetPanel(id="Panneau",
                tabPanel("Avec une base de données",
                         dashboardPage(dashboardHeader(title = "Base de données"),
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
                                                   mod_chargement_ui("chargement_1")
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
                         )
                         ),

                tabPanel("Saisie manuelle",
                         dashboardPage(dashboardHeader(title = "Saisie manuelle"),
                                       dashboardSidebar(
                                         sidebarMenu()
                                       ),
                                       dashboardBody()
                         )),



                tabPanel("Rédaction",
                         dashboardPage(dashboardHeader(title = "Rédaction"),
                                       dashboardSidebar(
                                         sidebarMenu()
                                       ),
                                       dashboardBody()
                         ))
                )




    )
}



# tagList(
#   # Leave this function for adding external resources
#   golem_add_external_resources(),
#   # Your application UI logic
#   tabsetPanel(id="Panneau",
#               tabPanel("Accueil",
#                        mod_chargement_ui("chargement_1")
#               ))
#
# )




#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gmrc"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )


}
