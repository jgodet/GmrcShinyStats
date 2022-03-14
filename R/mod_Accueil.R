#' Accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Accueil_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        tags$br(),                              
        img(src="logo1.png", height = "100%"	, width = "100%", style="display: block; margin-left: auto; margin-right: auto;"),
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
}
    
#' Accueil Server Functions
#'
#' @noRd 
mod_Accueil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$DLcsv <- downloadHandler(
      filename ='ExempleCSV.csv',
      content = function(file) file.copy('ExempleCSV.csv', file, overwrite = TRUE),
      contentType = 'application/csv'
      
    )
 
  })
}
    
## To be copied in the UI
# mod_Accueil_ui("Accueil_1")
    
## To be copied in the server
# mod_Accueil_server("Accueil_1")
