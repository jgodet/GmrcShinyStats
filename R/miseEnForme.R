pasDeBase <-  function(){
  fluidPage(
h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible" ),
p("Pour charger une base de données, rendez-vous sur l'onglet Base de Données en haut de cette page.")
)
}


###################################################
#####   PAGE 3      ###############################
###################################################
univarie <- function(){fluidPage(navbarPage(title = NULL,id="descriptif",

            tabPanel("Informations BDD",
                     fluidPage(
                       titlePanel("Informations sur la base de données"),

                       navlistPanel(
                         "Menu",
                         tabPanel("Informations brutes",

                                  fluidRow(
                                    splitLayout(cellWidths = c("30%","70%"),
                                                downloadButton('PDFdescriptif1o1',label="AIDE et Détails",class = "butt")
                                    )
                                  ),#finFluidRow
                                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                  verbatimTextOutput ("tableauBASE"),
                                  plotOutput('plotNAbase1')),
                         tabPanel("Données manquantes cumulées par variable",
                                  fluidRow(
                                    splitLayout(cellWidths = c("30%","70%"),
                                                downloadButton('PDFdescriptif1o2',label="AIDE et Détails",class = "butt"),
                                            h4("Faites attention s'il y a un filtre")
                                    )
                                  ),#finFluidRow

                                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                  h4("Descriptif cumulé des données manquantes par variable",align="center"),
                                  p("On représente ci-dessous les données manquantes en proportions par variable étudiée."),
                                  plotOutput('plotNAbase2'),
                                  tableOutput("tableNAbase2")),
                         tabPanel("Données manquantes cumulées par sujet",
                                  fluidRow(
                                    splitLayout(cellWidths = c("30%","70%"),
                                                downloadButton('PDFdescriptif1o3',label="AIDE et Détails",class = "butt"),
                                            h4("Faites attention s'il y a un filtre")
                                    )
                                  ),#finFluidRow

                                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                  h4("Descriptif cumulé des données manquantes par sujet",align="center"),
                                  p("On représente ci-dessous les données manquantes en proportions par sujet d'étude."),
                                  plotOutput('plotNAbase3'),
                                  tableOutput("tableNAbase3"))
                       )# fin navlistpanel

                     ) # fin fluipage

            ),# fin tabpanel




            ###################################################
            #####   PAGE 3.2     ###############################
            ###################################################



            tabPanel("Descriptif univarié",
                     fluidPage(
                       titlePanel("Analyses descriptives"),
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("propositions"),
                           radioButtons('qualiquanti', "Nature de la variable",
                                        c(Quantitative='quant', Qualitative='qual'),'qual'
                           )
                         ),
                         # Create a spot for the barplot
                         mainPanel(
                           # fluidRow(
                           #   # splitLayout(cellWidths = c("30%","70%"),
                           #   #             downloadButton('PDFdescriptif2',label="AIDE et Détails",class = "butt"),
                           #   #         h4("Faites attention s'il y a un filtre")
                           #   # )
                           # ),#finFluidRow

                           tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                           fluidRow(
                             column(6,   textOutput("descriptifUni"),br(),  tableOutput("descvar")),
                             column(6,     plotOutput('plot1') , plotOutput('plot2') )
                           )# fin fluid row du main panel

                         )# fin MainPanel

                       )# fin sidebarlayout
                     )# fin fluidpage
            )# fin tabPanel 2

)# fin navbarPage

)
}
#######################################################################
#####   PAGE 4 CROISEMENTS INFERENCE   ###############################
#######################################################################


CroisementsInference<-function(){
         fluidPage(navbarPage(id="Panel 2.x",title = NULL,

                     tabPanel("Croisement 2 à 2",


  fluidPage( includeCSS("./www/tables.css"),
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
}
############################################################
#####  page 5 survie              ##########################
############################################################

analyseDeSurvie<-function(){
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
        plotOutput('plotSURVIE'),
        p("Le détail des données utilisées pour la construction de cette ou ces courbes est présenté ci-dessous. Dans le cas
                                                        d'une comparaison entre plusieurs groupes, le détail est présenté par groupes, un test d'égalité de l'ensemble des courbes est
                                                        présenté (Test du Log-Rank) et les résultats sont affichés au bas de cette page."),
        h3("Valeurs numériques de survie: analyses détaillées"),
        verbatimTextOutput ("sortieSURVIE2"))# fin MainPanel

    )# fin sidebarlayout
  )# fin fluidpage

}
############################################################
#####  page 6 tests diagnostique  ##########################
############################################################


testsDiagnostiques<- function(){
  fluidPage(

navbarPage("",





            tabPanel("Réalisation d'un test diagnostique univarié",



                     fluidPage(
                       title = 'Examples of DataTables',
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

}


############################################################
#####  page 7 tests diagnostique    ########################
############################################################
concordanceAvecBase<-function(){
fluidPage(
  titlePanel("Analyse de concordance entre 2 lecteurs"),
  sidebarLayout(
    sidebarPanel(
      # si saisie par choix des variables
      conditionalPanel(
        condition = "input.CONCORsaisie == false",
        p("Sélectionnez la variable associée à la mesure du premier lecteur"),
        uiOutput("CONCORDANCElecture1"),
        br(),
        br(),
        p("Sélectionnez la variable associée à la mesure du deuxième lecteur"),
        uiOutput("CONCORDANCElecture2"),
        br(),
        br()),
      # quel type de saisie souhaité
      checkboxInput("CONCORsaisie", "Saisir les données des deux lecteurs manuellement"),
      # ajout de l'intervalle de confiance
      checkboxInput("CONCORinter", "Ajouter l'intervalle de confiance (simulations, le calcul peut prendre plusieurs minutes)"),
      # si saisie manuelle
      conditionalPanel(
        condition = "input.CONCORsaisie == true",
        p("Entrez les valeurs du lecteur 1, séparées par un espace"),
        textInput("Concoman1", label = "Valeurs du lecteur 1", value = ""),
        p("Entrez les valeurs du lecteur 2, séparées par un espace"),
        textInput("Concoman2", label = "Valeurs du lecteur 2", value = "")
      )# fin condi
    )# fin sidebar panel
    ,

    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("30%","70%"),
                    downloadButton('PDFconcordance',label="AIDE et Détails",class = "butt"),
                h4("Faites attention s'il y a un filtre")
        )
      ),#finFluidRow

      tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
      h3("Tableau croisé"),
      p("On présente ci-dessous le tableau croisé des lectures réalisées:"),
      tableOutput('mytableCONCORDANCE1'),br(),



      conditionalPanel(
        condition = "input.CONCORinter == true",
        verbatimTextOutput ("ConcordanceManuelleINTERV")
      ),# fin condi 1
      conditionalPanel(
        condition = "input.CONCORinter == false",
        verbatimTextOutput ("ConcordanceManuelleSimple")
      ),# fin condi 2
      p("Landis et Koch proposent l'interprétation suivante du coefficient Kappa de Cohen:"),
      tableOutput('LandisEtKoch2')



    )# fin MainPanel

  )# fin sidebarlayout
)# fin fluidpage

}

concordanceSansBase<- function(){
  fluidPage(
    titlePanel("Analyse de concordance entre 2 lecteurs"),
    sidebarLayout(
      sidebarPanel(
        p("Entrez les valeurs du lecteur 1, séparées par un espace"),
        textInput("Concoman1", label = "Valeurs du lecteur 1", value = ""),
        p("Entrez les valeurs du lecteur 2, séparées par un espace"),
        textInput("Concoman2", label = "Valeurs du lecteur 2", value = ""),
        checkboxInput("CONCORinter", "Ajouter l'intervalle de confiance (simulations, le calcul peut prendre plusieurs minutes)")       # si saisie manuelle


      )# fin sidebar panel
      ,

      mainPanel(
        fluidRow(
          splitLayout(cellWidths = c("30%","70%"),
                      downloadButton('PDFconcordance',label="AIDE et Détails",class = "butt"),
                  h4("Faites attention s'il y a un filtre")
          )
        ),#finFluidRow

        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        h3("Tableau croisé"),
        p("On présente ci-dessous le tableau croisé des lectures réalisées:"),
        tableOutput('mytableCONCORDANCE2'),br(),



        conditionalPanel(
          condition = "input.CONCORinter == true",
          verbatimTextOutput ("ConcordanceManuelleINTERV2")
        ),



        conditionalPanel(
          condition = "input.CONCORinter == false",
          verbatimTextOutput ("ConcordanceManuelleSimple2")
        ),# fin condi 2
        p("Landis et Koch proposent l'interprétation suivante du coefficient Kappa de Cohen:"),
        tableOutput('LandisEtKoch2')



      )# fin MainPanel

    )# fin sidebarlayout
  )# fin fluidpage
}





