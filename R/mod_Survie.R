#' Survie UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Survie_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
  uiOutput(ns('analyseDeSurvie'))
}
    
#' Survie Server Functions
#'
#' @noRd 
mod_Survie_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    source("./CodeSansDependance.R", local = TRUE)
    source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    eval(parse("./miseEnForme.R", encoding="UTF-8"))
    
    output$PDFsurvie = downloadHandler(
      filename    = '4_Survie.pdf',
      content     = function(file) file.copy('4_Survie.pdf', file, overwrite = TRUE),
      contentType = 'application/pdf'
    ) 

      
      observe({
    output$analyseDeSurvie = renderUI({
      if(!r$BASEchargee) do.call(tabPanel,pasDeBase)
      else do.call(tabPanel,analyseDeSurvie)
      
      
    })
    output$propositionsSURVIE1 <- renderUI({
      
      selectInput(ns("variablesurvie1"), "Variable délai",   choices=r$noms) 
    })
    
    output$propositionsSURVIE2 <- renderUI({
      
      selectInput(ns("variablesurvie2"), "Variable évenement 0/1:",   choices=r$noms[r$nbModeVariable==2]) 
    })
    
    output$propositionsSURVIE3 <- renderUI({
      noms2    <-r$noms[r$nbModeVariable<30]
      selectInput(ns("variablesurvie3"), "Variable groupe",   choices=noms2) 
    })
    
    output$plotSURVIE <- renderPlot({
      base    <-r$BDD
      
      variablesurvie1 <-base[,colnames(base)==input$variablesurvie1]
      variablesurvie2 <-base[,colnames(base)==input$variablesurvie2]
      variablesurvie3 <-base[,colnames(base)==input$variablesurvie3]
      if(!input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2      ) }
      if( input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2,variablesurvie3 ) }
    })
    
    
    output$sortieSURVIE2<- renderPrint({
      base    <-r$BDD
      variablesurvie1 <-base[,colnames(base)==input$variablesurvie1]
      variablesurvie2 <-base[,colnames(base)==input$variablesurvie2]
      variablesurvie3 <-base[,colnames(base)==input$variablesurvie3]
      if(!input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2      ) }
      if( input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2,variablesurvie3 ) }
    })
    
    })
    
    
  })
}
    
## To be copied in the UI
# mod_Survie_ui("Survie_1")
    
## To be copied in the server
# mod_Survie_server("Survie_1")
