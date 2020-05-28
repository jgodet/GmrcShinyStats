# concordanceSansBase.r
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


concordanceSansBase<-function(){
  a <-  fluidPage(

    titlePanel("Analyse de concordance entre 2 lecteurs"),
    sidebarLayout(
      sidebarPanel(
        p("Entrez les valeurs du lecteur 1, séparées par un espace"),
        textInput("Concoman1", label = "Valeurs du lecteur 1", value = ""),
        p("Entrez les valeurs du lecteur 2, séparées par un espace"),
        textInput("Concoman2", label = "Valeurs du lecteur 2", value = ""),
        checkboxInput("CONCORinter", "Ajouter l'intervalle de confiance (simulations, le calcul peut prendre plusieurs minutes)")       # si saisie manuelle


      ),# fin sidebar panel

      mainPanel(
        fluidRow(
          splitLayout(cellWidths = c("30%","70%"),
                      downloadButton("PDFconcordance",label="AIDE et Détails",class = "butt"),
                      h4("Faites attention s'il y a un filtre")
          )
        ),#finFluidRow

        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        h3("Tableau croisé"),
        p("On présente ci-dessous le tableau croisé des lectures réalisées:"),
        tableOutput("mytableCONCORDANCE2"),br(),



        conditionalPanel(
          condition = "input.CONCORinter == true",
          verbatimTextOutput ("ConcordanceManuelleINTERV2")
        ),



        conditionalPanel(
          condition = "input.CONCORinter == false",
          verbatimTextOutput ("ConcordanceManuelleSimple2")
        ),# fin condi 2
        p("Landis et Koch proposent l'interprétation suivante du coefficient Kappa de Cohen:"),
        tableOutput("LandisEtKoch2")



      )# fin MainPanel

    )# fin sidebarlayout
  )# fin fluidpage

  return(a)
}

