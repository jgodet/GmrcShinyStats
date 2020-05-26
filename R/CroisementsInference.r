# croisementsInference.r
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

if(!require(shiny)){install.packages('shiny')}
require(shiny)
croisementsInference<-fluidPage(navbarPage(id="Panel 2.x",title = NULL,

                       tabPanel("Croisement 2 a 2",

                                fluidPage( includeCSS("./inst/shiny-examples/myapp/www/tables.css"),
                                           titlePanel("Analyses descriptives croisées"),
                                           sidebarLayout(
                                             sidebarPanel(

                                               uiOutput("propositionsCROISE1"),
                                               radioButtons('qualiquantiCROISE1',
                                                            "Nature de la variable",
                                                            c(Quantitative='quant', Qualitative='qual'),
                                                            'quant'),
                                               uiOutput("propositionsCROISE2"),
                                               radioButtons('qualiquantiCROISE2',
                                                            "Nature de la variable",
                                                            c(Quantitative='quant', Qualitative='qual'),
                                                            'quant'),
                                               conditionalPanel(
                                                 condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'",
                                                 radioButtons('NATableau',
                                                              "Afficher les données manquante",
                                                              c(Non="no", Oui='always'),
                                                              "no"))



                                             ),
                                             mainPanel(
                                               fluidRow(
                                                 splitLayout(cellWidths = c("30%","70%"),
                                                             downloadButton('PDFcroisements',label="AIDE et Détails",class = "butt"),
                                                             h4("Faites attention s'il y a un filtre")
                                                 )
                                               ),#finFluidRow

                                               tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                               h3("Représentation graphique du lien entre les deux variables"),
                                               plotOutput('plotCROISE' ),
                                               # debut conditionnal panel QualiQuali
                                               conditionalPanel(
                                                 condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'",
                                                 h3("Tableau croisé",align = "left",style = "color:#08088A"),
                                                 tableOutput("montableauCroisAUTO"),br(),
                                                 tableOutput("montableauCroise2AUTO"),
                                                 tableOutput("montableauCroise3AUTO"),
                                                 h3("Tests d'association / Comparaison des proportions",align = "left",style = "color:#08088A"),
                                                 fluidRow(
                                                   splitLayout(cellWidths = c("50%","50%"),
                                                               tableOutput('AUTOtableCHI2'),
                                                               tableOutput('AUTOtableFISHER')
                                                   )
                                                 ),
                                                 textOutput('AUTOCHI2conditions'),
                                                 h3("Rapport de cotes",align = "left",style = "color:#08088A"),
                                                 tableOutput('oddratioAUTO')
                                               ),# fin panelQualiQuali,
                                               # debut conditionnal panel QuantiQuali
                                               conditionalPanel(
                                                 condition = "input.qualiquantiCROISE1 != input.qualiquantiCROISE2",
                                                 h3("Descriptif complet",align = "left",style = "color:#08088A"),
                                                 tableOutput('descr3DESCRIPTIF'),
                                                 h3("Tests de comparaisons:",align = "left",style = "color:#08088A"),
                                                 verbatimTextOutput ("descr3TestNormalite"),
                                                 verbatimTextOutput ("descr3Testpv"),
                                                 verbatimTextOutput ("descr3TestsNPv"),
                                                 verbatimTextOutput ("descr3Tests_de_Student"),
                                                 verbatimTextOutput("descr3TestsMANN"),
                                                 verbatimTextOutput ("ChoixSortieCROISE")
                                               ), # fin Panel Quali Quanti
                                               # debut conditionnal panel QuantiQuanti
                                               conditionalPanel(
                                                 condition = "input.qualiquantiCROISE1 == 'quant' && input.qualiquantiCROISE2 == 'quant'",
                                                 h3("Corrélation entre deux variables quantitatives",align = "left",style = "color:#08088A"),
                                                 verbatimTextOutput ("CorrelationCROISE")
                                               ),# fin panelQuantiQuali,
                                               plotOutput('plotCROISE2')
                                             )# fin MainPanel

                                           )# fin sidebarlayout
                                ))# fin fluidpage
                       ,

                       tabPanel("Tableau croisement",



                                fluidPage(
                                  titlePanel("Analyses descriptives croisées"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      #
                                      uiOutput("propositionsTableauCROISE"),
                                      uiOutput("selectionVariablesCroisees1"),
                                      uiOutput("selectionVariablesCroisees3"),
                                      uiOutput("selectionVariablesCroisees2"),
                                      radioButtons("tableauCroiseSimpli","Tableau avec abréviation :"
                                                   , c( Oui = 1, Non = 0),0),
                                      sliderInput("nbDec", "Nombre de decimales : ", min =0,
                                                  max = 5, value= 3, step = 1),
                                      downloadButton('downloadData', 'Télécharger la table')


                                    ),
                                    mainPanel(
                                      #   fluidRow(
                                      #
                                      #                 downloadButton('PDFcroisements',label="AIDE et Détails",class = "butt")
                                      #
                                      #
                                      # ),#finFluidRow

                                      # tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                      h3("Tableau de comparaison de population"),
                                      conditionalPanel(condition = "!is.null(input$VariableCroisees)", tableOutput('tableauCroisement'))

                                    )# fin MainPanel

                                  )# fin sidebarlayout
                                )# fin fluidpage
                       ) # fin tabPanel tableau Croisement
  ) # fin tabset
  )
