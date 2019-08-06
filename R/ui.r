# ui.r
# written by JuG
# August 01 2019


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
if(!require('shiny')){install.packages('shiny')}
require(shiny)
# Define UI for app that draws a histogram ----

ui <- shinyUI(navbarPage(
    "G.M.R.C",

    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    #######     SHINY AVEC UNE BASE DE DONNEES      ##################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################



    ###################################################
    #####   PAGE 1      ###############################
    ###################################################
    tabPanel("Avec une base de données",

             tabsetPanel(id = "Panneau",

                         tabPanel("Accueil",
                                  fluidPage(
                                    sidebarLayout(
                                      sidebarPanel(
                                        tags$br(),
                                        img(src="./www/logo1.png", height = "100%"	, width = "100%", style="display: block; margin-left: auto; margin-right: auto;"),
                                        tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                        tags$br(),tags$br(),
                                        tags$h4("En cas de question, vous pouvez contacter:",style = "color:#08088A"),
                                        tags$p("GMRC CHU Strasbourg",style = "color:#08088A"),
                                        tags$p("ShinyGMRC@chru-strasbourg.fr",style = "color:#08088A"),
                                        # tags$p("03 88 1(1 67 49)",style = "color:#08088A"),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()
                                        #height = 400	, width = 492
                                        tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(), tags$br(),tags$br(),tags$br(),tags$br()),
                                      mainPanel(
                                        tags$h2(tags$u("Descriptifs et analyses statistiques"),align = "center",style = "color:#08088A"),
                                        tags$br(),
                                        tags$p("Cette application est proposée par le Groupe Méthode en Recherche Clinique des hôpitaux universitaires de Strasbourg, pour le descriptif de variables quantitatives et qualitatives de votre base de données.
                                             Les onglets en haut de cette page sont à parcourir dans le sens de lecture. Une base de données saisie dans un tableau (Excel, OppenOffice) devra être chargée sur la page avant toute utilisation des onglets statistiques.
                                             Une fois la base de données chargée dans la mémoire de l'application,
                                             vous serez en mesure de réaliser des descriptifs complets ainsi que des analyses graphiques de l'ensemble de vos variables,
                                             qu'elles soient de nature qualitative ou quantitative.
                                             Vous utiliserez alors le logiciel de statistiques R, sans avoir à programmer informatiquement les commandes à passer."),
                                        tags$br(),

                                        tags$h4("En cas de problèmes:",align = "center",style = "color:#08088A"),
                                        tags$p("Si vous rencontrez des messages d'erreur ou que certaines fonctionnalités ne s'exécutent pas, ceci peut être dû à l'une des causes suivantes:"),

                                        tags$ul(tags$li("Vérifiez que votre base est au bon format. Un fichier CSV ou un fichier TXT de type séparateur = tabulation"),
                                                tags$li("Vérifiez que toutes les cases de votre tableau soient remplies. On parle de tableau rectangulaire plein."),
                                                tags$li("Vérifiez que les variables quantitatives ne possèdent pas de texte (type commentaires), ni de caractères spéciaux et que les variables qualitatives ne possedent pas de valeur quantitative")
                                        ),# find ul
                                        tags$br(),
                                        tags$p("En cas de doute, ou si un problème persiste, assurez-vous d'avoir respecté toutes les modalités du fichier de recommantation, traitant du format d'une base, que vous pouvez télécharger ici:"),
                                        tags$br(),tags$br(),
                                        column(6,
                                               strong("* Instructions sur le format d'un tableau : saisie des données"),
                                               downloadButton('formatBASE',label="Télécharger Instructions format d'une base de données",class = "BOUTON"),
                                               tags$head(tags$style(".BOUTON{background-color:#F1F1F7;} .BOUTON{color: black;}")),
                                               tags$br(),tags$br(),tags$br(),
                                               "* Un exemple de fichier au format CSV adéquat est téléchargeable ici:",
                                               downloadButton('DLcsv',label="Télécharger Exemple de base de données",class = "BOUTON"),
                                               tags$head(tags$style(".BOUTON{background-color:#F1F1F7;} .BOUTON{color: black;}")),
                                               tags$br(),tags$br(),  tags$br(),	tags$br()),
                                        column(6, img(src="tableau.PNG", height = 176, width = 500),tags$br(),  tags$br(),	tags$br()),



                                        tags$h4("Tutoriels d'utilisation en vidéo",style = "color:#08088A",align = "center"),
                                        tags$p("De brèves vidéos explicatives sont disponibles en ligne, en cliquant sur le bouton ci-dessous:"),

                                        tags$br(),
                                        tags$p(tags$a(img(src="Miniature.jpeg", height = 94, width = 170), target = "_blank",
                                                      href =    "https://www.youtube.com/watch?v=p1TsN3LeQI4&list=PLRC56KyFX6kk3FJ6FuwvTFRd3ZoNFSLlG"),align = "center"),
                                        tags$h4("Note",style = "color:red",align = "left"),
                                        tags$p("Cette application n'enregistre ni votre activité, ni vos données. Autrement dit, à la fermeture de l'application, vous ne
                                             pourrez pas revenir ou récupérer les analyses effectuées. Il vous faudra recharger la base et recommencer les manipulations.",style = "color:red"),
                                        tags$h2("Note",style = "color:white",align = "center"),
                                        tags$h4("Note",style = "color:black"),
                                        tags$p("Vous êtes responsable de la conformité réglementaire et de la sécurité des données que vous exploitez au travers de l’application «GMRC Shiny Stats».
												L’application doit donc être utilisée conformément aux règles de sécurité établies par votre structure.
												Pour toute mise en œuvre d’une base de données directement ou indirectement nominatives,
												il convient de se mettre en conformité avec la loi Informatique et Libertés.
												Pour tout complément d’information par rapport à la loi « Informatique et Libertés »,
												pour toute demande de déclaration auprès de la CNIL ou pour toute question relative à la sécurité des données, pour les HUS,
												vous pouvez  contacter l’équipe sécurité du CRIH par courriel à securite-si@chru-strasbourg.fr.
												Pour les autres structures, il conviendra de vous rapprocher de votre Correspondant Informatique et Libertés ou de votre service informatique."),
                                        tags$br(),  tags$br()


                                      ) # fin Main Panel
                                    ) # Fin SideBarLayout
                                  ) # fin fluidPage
                         ),# fin TapPANEL,




                         ###################################################
                         #####   PAGE 2      ###############################
                         ###################################################

                         tabPanel("Base de données",
                                  fluidPage(
                                    titlePanel("Charger un fichier"),
                                    sidebarLayout(
                                      sidebarPanel(




                                        shinyFilesButton("Btn_GetFile", "Chercher la base" ,
                                                         title = "Selectionner la base de données:", multiple = FALSE,
                                                         buttonType = "default", class = NULL),

                                        textOutput("txt_file")    ,
                                        tags$br(),

                                        # textOutput("essai"),
                                        # fileInput('file1', 'Choisissez un jeu de données à charger (extension txt ou csv)',
                                        #           accept=c('text/csv',
                                        #                    'text/comma-separated-values,text/plain',
                                        #                    '.csv')),
                                        # actionButton("goButton", "Go!"),
                                        tags$hr(),
                                        checkboxInput('header', 'Titre       (Votre fichier contient-il des titres de colonnes ?)', TRUE),
                                        radioButtons('sep', 'Séparateur',
                                                     c(Virgule=',',
                                                       `Point Virgule`=';',
                                                       Tabulation='\t'),
                                                     ';'),
                                        radioButtons('manquants', 'Manquants (Quel symbole est utilisé pour les données manquantes ?)',
                                                     c(Slash='/',
                                                       Etoile='*',
                                                       `Lettres NA`='NA'),
                                                     '*'),
                                        radioButtons('decimale', 'Symbole de décimale',
                                                     c(Virgule=',',
                                                       Point='.'),
                                                     ','),
                                        tags$br(),
                                        actionButton("upload", "Charger/actualiser la base",class = "btn-success") ,
                                        radioButtons('encodage', "Si vous avez des problèmes d'import de la base de données, potentiellement liés à l'encodage",
                                                     c(`Windows/Excel`='windows-1252',
                                                       `Linux/LibreOffice` ='utf-8'),
                                                     'windows-1252'),

                                        tags$br(),tags$br(),tags$br(),
                                        "Les lignes entièrement vides seront retirées pour la suite des analyses."

                                      ),
                                      mainPanel(
                                        fluidRow(
                                          splitLayout(
                                            downloadButton('PDFbase',label="AIDE et Détails",class = "butt"),
                                            tags$h4("Faites attention s'il y a un filtre")

                                          )
                                        ),#finFluidRow
                                        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),tags$br(),
                                        DT::dataTableOutput("contents2"),
                                        tags$div(textOutput("BASEchargee"), style = "color:white")


                                        #tableOutput('contents')
                                      ) # fin Main panel
                                    )   # fin sidebarlayout
                                  )     # fin fluidpage
                         ) ,    # fin tabPanel 1






                         ###################################################
                         #####   PAGE 3.1     ###############################
                         ###################################################

                         tabPanel("Descriptifs",
                                  uiOutput('univarie')
                         ),# fin tab panel descriptif


                         #######################################################################
                         #####   PAGE 4  CROISEMENTS INFERENCE   ###############################
                         #######################################################################
                         tabPanel("Croisements/Inférence",
                                  uiOutput('CroisementsInference')
                         ),#fin table panel





                         ###################################################
                         #####   PAGE 5     ###############################
                         ###################################################


                         tabPanel("Analyse de survie",
                                  uiOutput('analyseDeSurvie')
                         ),# fin tabPanel 3



                         ###################################################
                         #####   Page 6 TEST DIAGNOSTIQUES    ################
                         ###################################################

                         tabPanel("Tests Diagnostiques",
                                  uiOutput('testsDiagnostiques')
                         ),






                         ###################################################
                         #####   Page 7 CONCORDANCE           ##############
                         ###################################################


                         tabPanel("Concordance",
                                  uiOutput('concordance')
                         )# fin tabPanel 3


             )
    ),



    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    #######     SHINY SANS  BASE DE DONNEES      ##################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################
    ##################################################################


    tabPanel("Saisie manuelle",


             tabsetPanel(id="Panneau2",
                         tabPanel("Tableau croisé",
                                  fluidPage(
                                    titlePanel("Réaliser un tableau croisé"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        numericInput("NbLignesMAIN", "Nombre de lignes", 2),
                                        numericInput("NbcolonnesMAIN", "Nombre de colonnes", 2),
                                        tags$p("Entrez les valeurs agrégées du tableau, séparées par un espace (remplissage en colonne)"),
                                        textInput("TableauMAIN1", label = "Effectifs du tableau:", value = "1 2 3 4")

                                      ),


                                      mainPanel(
                                        tags$h3("Tableau croisé",align = "left",style = "color:#08088A"),
                                        tags$p("On présente ci-dessous le tableau croisé des deux variables:"),
                                        fluidRow(
                                          splitLayout(cellWidths = c("30%","30%","30%"),
                                                      tableOutput('montableauCroisemanuel'),
                                                      tableOutput('montableauCroisemanuel2'),
                                                      tableOutput('montableauCroisemanuel3')

                                          )
                                        ),# fin fluid row
                                        tags$h3("Tests d'association / Comparaison des proportions",align = "left",style = "color:#08088A"),
                                        fluidRow(
                                          splitLayout(cellWidths = c("50%","50%"),
                                                      tableOutput('MAINtableCHI2'),
                                                      tableOutput('MAINtableFISHER')
                                          )
                                        ),

                                        # spécialités pour les tableaux 2x2
                                        conditionalPanel(
                                          condition = "input.NbLignesMAIN=='2' && input.NbcolonnesMAIN=='2'",

                                          textOutput('CHI2conditions'),
                                          tags$h3("Rapport de cotes / Différence de proportions",align = "left",style = "color:#08088A"),
                                          fluidRow(
                                            splitLayout(cellWidths = c("50%","50%"),
                                                        tableOutput('oddratioMAIN'),
                                                        tableOutput('DiffDesProps')
                                            )
                                          ),
                                          tags$h3("Performance diagnostique",align = "left",style = "color:#08088A"),
                                          tags$p("Le prochain tableau présente les critères de performances diagnostiques de X sur la valeur de Y"),
                                          tableOutput('PerforMAIN')

                                        ),#fin conditional panel des tableaux 2x2


                                        tags$h3("Concordance / Kappa",align = "left",style = "color:#08088A"),
                                        tags$p("La concordance est évaluée à l'aide du coefficient Kappa de Cohen, et est estimé ci-dessous:"),
                                        fluidRow(
                                          splitLayout(cellWidths = c("25%","55%","20%"),
                                                      tableOutput('KappaMAIN'),
                                                      plotOutput('KappaPlotMain'),
                                                      tableOutput('LandisEtKoch')
                                          )# fin SpliLayout Kappa

                                        )# fin fluiRow pour Kappa

                                      )   # fin Main panel
                                    )   # fin sidebarlayout
                                  )   # fin fluidpage
                         )
                         #fin tabpanel 1

             )# fin tabsetPannel
    ),# fin panneau saisie manuelle




    #####################################################################
    ####         DEBUT DE LA PAGE DE REDACTION DES RESULTATS      #######
    #####################################################################

    tabPanel("Rédaction",
             tabPanel("Rédation et présentation des résultats",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            tags$br(),
                            img(src="https://github.com/jgodet/GmrcShinyStats/blob/master/R/www/logo1.png", height = "100%"	, width = "100%", style="display: block; margin-left: auto; margin-right: auto;"),
                            tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                            tags$br(),tags$br(),
                            tags$h4("En cas de question, vous pouvez contacter:",style = "color:#08088A"),
                            tags$p("GMRC CHU Strasbourg",style = "color:#08088A"),
                            tags$p("ShinyGMRC@chru-strasbourg.fr",style = "color:#08088A"),
                            # tags$p("03 88 1(1 67 49)",style = "color:#08088A"),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()
                            #height = 400	, width = 492
                            tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(), tags$br(),tags$br(),tags$br(),tags$br()),


                          mainPanel(
                            tags$h3("Rédiger la partie matériel et méthodes",align = "left",style = "color:#08088A"),
                            tags$p("On présente ci-dessous un exemple de rédaction de la partie « matériel et méthodes ». C'est exemple doit être adapté et modifié en fonction du type d'étude réalisée et du contexte. Le premier paragraphe correspond à un exemple type pour une utilisation des outils descriptifs, des croisements de variables et des tests d'associations univariés. Les parties suivantes sont spécifiques aux analyses de performances diagnostiques et aux mesures de concordance. Elles pourront être ajoutées à l'endroit symbolisé par deux astérisques/étoiles **"),
                            tags$br(),tags$br(),
                            tags$h4("Matériel et méthodes",align = "left",style = "color:#08088A"),
                            tags$p(tags$i("Les variables quantitatives ont été décrites à l'aide des statistiques usuelles de position et de dispersion, à savoir la moyenne, la médiane, la variance, le minimum, le maximum et les quantiles. Les variables qualitatives ont été quant à elles décrites avec les effectifs et les proportions de chaque modalité. Des proportions cumulées ont également été calculées pour les variables à plus de deux modalités."),style = "color:#08088A"),
                            tags$p(tags$i("Le caractère Gaussien des variables quantitatives a été évalué à l'aide du test de Shapiro-Wilk. Si les conditions d'application étaient respectées, le lien entre deux variables quantitatives a été évalué à l'aide du test de corrélation linéaire de Pearson. Dans le cas contraire, un test de corrélation de Spearman a été réalisé. Pour la comparaison d une variable quantitative entre plusieurs sous-groupes, une analyse de la variance ou le test de Kruskal et Wallis ont été utilisés, toujours en fonction des hypothèses d'utilisation de chacun de ces tests. Enfin pour le croisement entre plusieurs variables qualitatives, le test paramétrique du Chi2 a été utilisé si les conditions d application le permettaient. Si ce n était pas le cas, le test exact de Fisher a été réalisé."),style = "color:#08088A"), tags$p(tags$i("**"),style = "color:#08088A"),
                            tags$p(tags$i("Le risque de première espèce alpha a été fixé à 5% pour toutes les analyses. L'ensemble des analyses a été réalisé sur le logiciel R dans sa version 3.1, R Development Core Team (2008). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. via l’application GMRC Shiny Stat du CHU de Strasbourg (2017)"),style = "color:#08088A"),
                            tags$br(),tags$br(),tags$br(),tags$br(),
                            tags$h5("** Performances diagnostiques",align = "left",style = "color:#08088A"),
                            tags$p(tags$i("Pour évaluer les performances diagnostiques d'un critère binaire sur une variable dépendante, les mesures de sensibilité, de spécificité, la valeur prédictive positive et la valeur prédictive négative ont été estimées à partir du tableau de contingence associé.")),
                            tags$h5("** ROC",align = "left",style = "color:#08088A"),
                            tags$p(tags$i("Pour évaluer les performances diagnostiques d'une variable quantitative, une recherche du seuil optimal a été réalisée en utilisant une courbe ROC. Les sensibilités et spécificités ont été évaluées pour différents seuils et le seuil optimal a été défini par maximisation de l'indice de Youden. L'intervalle de confiance de la courbe ROC ainsi que le test de la différence de l'aire sous la courbe à 0.50 a été estimé par simulations bootstrap sur les mesures de performance du critère. Pour le seuil alors défini, les critères de performance habituels ont été évalués à savoir : la sensibilité, la spécificité, les valeurs prédictives positive et négative.")),
                            tags$h5("** Concordance",align = "left",style = "color:#08088A"),
                            tags$p(tags$i("La concordance entre deux lectures qualitatives a été évaluée par le coefficient Kappa de Cohen. Le coefficient est présenté sur une échelle [-1;1] allant de façon continue de la discordance à la concordance totale. L'intervalle de confiance associé est évalué par 1000 réplications bootstrap, et la méthode choisie est l'estimation de l'intervalle de type bca (bias corrected and accelerated, Efron 1987).")),
                            tags$h5("** Survie",align = "left",style = "color:#08088A"),
                            tags$p(tags$i("Une analyse de survie a été réalisée en considérant le délai jusqu'à apparition de l'évènement. Les sujets n'ayant pas présenté l'évènement étaient censurés à la date des dernières nouvelles. Dès lors, une courbe de Kaplan-Meier a permis d'étudier l'évolution du taux de survie en fonction du temps dans son intervalle de confiance associé. Une comparaison entre plusieurs groupes de l'évolution de la survie a été réalisée en utilisant le test du Log-Rank associé aux courbes de Kaplan-Meier."))
                          )   # fin Main panel
                        )   # fin sidebarlayout
                      )   # fin fluidpage
             )

    )








  )# fin navbarpage
)

