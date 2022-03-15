#' chargement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chargement_ui <- function(id){
  ns <- NS(id)
  tagList(
 h1("Hello chargement !")
  )
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE)
      ),
      mainPanel(
        tableOutput("contents")
      )
    )
  )
}

#' chargement Server Functions
#'
#' @noRd
mod_chargement_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_chargement_ui("chargement_1")

## To be copied in the server
# mod_chargement_server("chargement_1")
