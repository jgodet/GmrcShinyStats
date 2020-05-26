# testsDiagnostiques.r
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


testsDiagnostiques<-
  fluidPage(

    navbarPage("",
     tabPanel("Réalisation d'un test diagnostique univarié",
                        fluidPage(
                          title = "Examples of DataTables",
                          sidebarLayout(
                            sidebarPanel(
                              p("Sélectionnez la variable qualitative codée 0 ou 1 à expliquer:"),
                              uiOutput("propositionsLOGIT1"),
                              p("Sélectionnez la variable quantitative explicative"),
                              uiOutput("propositionsLOGIT2"),
                              br(),
                              br()


                            ), # fin sidebar panel
                            mainPanel(
                              fluidRow(
                                splitLayout(cellWidths = c("30%","70%"),
                                            downloadButton('PDFdiag',label="AIDE et Détails",class = "butt"),
                                            h4("Faites attention s'il y a un filtre")
                                )
                              ),#finFluidRow

                              tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                              navbarPage(title=NULL,
                                         id='datasetlogit',
                                         tabPanel('Variables sélectionnées',
                                                  h3("Variables sélectionnées"),
                                                  p("Les variables sélectionnées pour la réalisation du test diagnostique sont:"),
                                                  tableOutput('mytableLOGIT1')
                                         ),
                                         tabPanel('Courbe ROC',
                                                  p("La courbe ROC réalisée à partir des variables sélectionnées est présentée ci-dessous"),
                                                  checkboxInput("LOGIToptionsGRAPHIQUES", "Je souhaite ajouter des options graphiques", FALSE),
                                                  conditionalPanel(
                                                    condition = "input.LOGIToptionsGRAPHIQUES",
                                                    checkboxInput("LOGIToptionsAUC", "Afficher Aire sous la courbe sur le graphique", FALSE),
                                                    checkboxInput("LOGIToptionsSEUIL", "Afficher seuil optimal sur le graphique", FALSE),
                                                    checkboxInput("LOGIToptionsIntervalle", "Afficher intervalle de confiance courbe ROC", FALSE)
                                                  ),
                                                  h3("Courbe ROC associée"),
                                                  plotOutput('LogitROC'),
                                                  br(),br(),
                                                  h3("Meilleur seuil estimé par maximisation de l'indice de Youden"),
                                                  tableOutput('LogitROCtableauBEST'),
                                                  br(),br(),
                                                  h3("Détails des seuils utilisés pour construire la courbe"),
                                                  tableOutput('LogitROCtableau')

                                         ), # fin tab panel
                                         tabPanel('Performances diagnostiques',
                                                  h4("Le meilleur seuil (au sens défini précédemment) est estimé à:"),
                                                  tableOutput('LogitPERFtableauBEST'),br(),
                                                  h4("Pour un tel seuil le tableau croisé devient:"),
                                                  tableOutput('LogitPERF3'),br(),
                                                  h4("La sensibilité et la specificité sont:"),
                                                  tableOutput('LogitPERF1'),br(),
                                                  h4("La critères de performances sont alors"),
                                                  tableOutput('LogitPERF2'),
                                                  p("Attention, si l'évènement est associé à une mesure inférieure au cut, la lecture des VP,VN,FP,FN, VPP et VPN est inversée dans ce dernier tableau.
                                                                             Il faut alors se référer au tableau à 4 cases sur le haut de cette page."),br()



                                         )
                              )# fin tab set panel
                            ) # fin main panel
                          ) # fin sidebarlayout
                        ) # fin fluipage   )
               ) # fin tabpanel

    )
  )



