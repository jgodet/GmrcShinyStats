# analyseDeSurvie.r
# written by JuG
# August 05 2019


#' Do something
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'
#'
#' @return
#' @export

require(shiny)

analyseDeSurvie <-

  fluidPage(

    titlePanel("Analyses de survie"),
    sidebarLayout(
      sidebarPanel(
        p("Sélectionnez la variable quantitative représentant le délai jusqu'à survenue de l'évènement ou de censure"),
        uiOutput("propositionsSURVIE1"),
        p("Sélectionnez la variable qualitative codée 0 si censure ou 1 si survenue de l'évènement"),
        uiOutput("propositionsSURVIE2"),
        br(),
        br(),
        checkboxInput("SURVIEcompar", "Faire une comparaison inter-groupes"),

        conditionalPanel(
          condition = "input.SURVIEcompar ",
          p("Sélectionnez la variable qualitative représentant les différents sous-groupes:"),
          uiOutput("propositionsSURVIE3")
        )# fin condi
      )# fin sidebar panel
      ,

      mainPanel(
        fluidRow(
          splitLayout(cellWidths = c("30%","70%"),
                      downloadButton('PDFsurvie',label="AIDE et Détails",class = "butt"),

                      h4("Faites attention s'il y a un filtre")

          )
        ),#finFluidRow

        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        h3("Courbe(s) de Kaplan-Meier"),
        p("La courbe de survie associée aux variables selectionnée est présentée ci-dessous. Si aucune comparaison entre groupes n'est
                                                        effectuée, la courbe est présentée dans son intervalle de confiance à 95%. Si une comparaison est demandée, le graphique présente
                                                        la courbe de Kaplan-Meier dans chacun des groupes."),
        plotOutput("plotSURVIE"),
        p("Le détail des données utilisées pour la construction de cette ou ces courbes est présenté ci-dessous. Dans le cas
                                                        d'une comparaison entre plusieurs groupes, le détail est présenté par groupes, un test d'égalité de l'ensemble des courbes est
                                                        présenté (Test du Log-Rank) et les résultats sont affichés au bas de cette page."),
        h3("Valeurs numériques de survie: analyses détaillées"),
        verbatimTextOutput ("sortieSURVIE2"))# fin MainPanel

    )# fin sidebarlayout
)# fin fluidpage
