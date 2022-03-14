#' Base_de_donnees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyFiles
mod_Base_de_donnees_ui <- function(id){
  ns <- NS(id)
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
  )
}
    
#' Base_de_donnees Server Functions
#'
#' @noRd 
mod_Base_de_donnees_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    volumes = getVolumes()
    observe({  
      shinyFileChoose(input, "Btn_GetFile", roots = volumes, session = session)
      
      if(!is.null(input$Btn_GetFile)){
        # browser()
        file_selected<-parseFilePaths(volumes, input$Btn_GetFile)
        output$txt_file <- renderText(as.character(file_selected$datapath))
      }
    })
    
    
    # output$essai <-renderText({ input$path}) # Visualisation du chemin d'acces
    contentInput <- reactive({ 
      
      if(input$upload == 0) return()
      
      isolate({
        file_selected<-parseFilePaths(volumes, input$Btn_GetFile)
        
        D<-read.csv(paste(as.character(file_selected$datapath), collapse = "\n"), header=input$header,sep=input$sep, na.string=c("",input$manquants),dec=input$decimale, fileEncoding = input$encodage)
        lignesVides<-apply(D,1,function(x){sum(is.na(x))})==dim(D)[2]
        D<-D[!lignesVides,]
        return(D)
      })
    })
    
    output$contents2 <- DT::renderDataTable(DT::datatable({contentInput() },filter = 'top')
    )
    output$dat_false <- DT::renderDataTable(DT::datatable({contentInput() },filter = "top"),server = FALSE)
    
    BDD      <- reactive({  
      # D<-read.csv2( 'C:/Users/fabachet/Desktop/GMRC Shiny Stats/GMRC Shiny Stats/Shiny/BDD.csv', na.strings =  "*")
      # 
      D<-contentInput()
      D<-D[input$contents2_rows_all,]
      lignesVides<-apply(D,1,function(x){sum(is.na(x))})==dim(D)[2]		
      D<-D[!lignesVides,]
      D
    })
    
    
    BASEchargee<-reactive({
      CHARGEE<- tryCatch(dim(BDD())[1]>0,warning= function(e) F,error= function(e) F)
      
      CHARGEE
      
    })
    
    
    #   ANCIENNE FACON DE CHARGER UNE BASE DE DONNEES
    # # la base de données en data frame pour affichage au chargement et manipulation
    #   output$contents2 <- DT::renderDataTable(DT::datatable({
    #     inFile <- input$file1
    #     if (is.null(inFile))      return(NULL)
    #     D<-read.csv(inFile$datapath, header=input$header,sep=input$sep, na.string=c("",input$manquants),dec=input$decimale)
    #   },filter = 'top')
    # )
    # 
    # La base de données en objet réactif pour manipuler dans tout le fichier server.R
    # BDD      <- reactive({
    #   inFile  <- input$file1
    #   D<-read.csv(inFile$datapath, header=input$header,sep=input$sep, na.string=c("",input$manquants),dec=input$decimale)
    #   # D<-D[input$contents2_rows_all,]
    #   lignesVides<-apply(D,1,function(x){sum(is.na(x))})==dim(D)[2]
    #   D<-D[!lignesVides,]
    #   D
    # })
    noms<- reactive({
      base<- BDD()
      colnames(base) 
    })
    
    nbSujet<-reactive({
      res<-dim(contentInput())[1]
      print(res)
      res
      
    })
    
    #Recherche du nombre de modalités par variable
    nbModeVariable <- reactive({
      D <- BDD()
      ret <- NULL
      for(i in 1 : (dim(D)[2])){
        ret[i] <- nlevels(as.factor(D[,i]))
      }
      ret
    })
    
    
    variableNum <- reactive({
      D <- BDD()
      ret <- NULL
      for(i in 1 : (dim(D)[2])){
        ret[i] <- is.numeric(D[,i])
      }
      ret
    })
    
    variableNormale <- reactive({
      D <- BDD()
      ret <- rep(NA, dim(D)[2])
      
      for(i in which(variableNum())){
        ret[i] <- desctable::is.normal(D[,i])
      }
      ret
    })
    
    
    
    
    # Est-ce qu'un filtre est appliqué à la base de données ?
    output$FILTREapplique44<- renderUI({
      
      
      HTML("f")
      
      
    })
 
  })
}
    
## To be copied in the UI
# mod_Base_de_donnees_ui("Base_de_donnees_1")
    
## To be copied in the server
# mod_Base_de_donnees_server("Base_de_donnees_1")
