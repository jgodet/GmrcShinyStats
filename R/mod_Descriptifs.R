#' Descriptifs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Descriptifs_ui <- function(id){
  ns <- NS(id)
  uiOutput('univarie')
}
    
#' Descriptifs Server Functions
#'
#' @noRd 
mod_Descriptifs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ########################################################################################################################
    ####    OUTPUT page 3 : Descriptif de la base et NA          
    ########################################################################################################################
    
    output$univarie = renderUI({
      if(!BASEchargee()) do.call(tabPanel,pasDeBase)
      else do.call(tabPanel,univarie)
      
      
    })
    
    
    output$plotNAbase1 <- renderPlot({
      plot.na(BDD())
    })
    
    output$plotNAbase2 <- renderPlot({
      barplot(apply(is.na(BDD()),2,sum),xlab="", col = "palegreen3")
    })
    
    output$plotNAbase3 <- renderPlot({
      barplot(apply(is.na(BDD()),1,sum),xlab="",  col ="lightblue1")
    })
    
    output$tableauBASE <- renderPrint({
      descd(BDD())
      
    })
    
    output$tableNAbase2 <- renderTable({
      D           <-BDD()
      NbVariables <-dim(D)[2]
      matriceNA   <-matrix(NA,nrow=NbVariables,ncol=3)
      for(i in 1:NbVariables){
        matriceNA[i,1]<-round(sum   (is.na(D[,i])))
        matriceNA[i,2]<-round(length(is.na(D[,i])))
        matriceNA[i,3]<-round(sum   (100*is.na(D[,i]))/length(is.na(D[,i])),2)
      }
      colnames(matriceNA)<-c("Nb.manquants","Nb.données","%")
      rownames(matriceNA)<-colnames(D)
      matriceNA 
    },rownames=TRUE)
    
    output$tableNAbase3 <- renderTable({
      D           <-t(BDD())
      NbVariables <-dim(D)[2]
      matriceNA   <-matrix(NA,nrow=NbVariables,ncol=3)
      for(i in 1:NbVariables){
        matriceNA[i,1]<-round(sum               (is.na(D[,i])))
        matriceNA[i,2]<-round(length            (is.na(D[,i])))
        matriceNA[i,3]<-round(sum               (100*is.na(D[,i]))/length       (is.na(D[,i])),2)
      }
      colnames(matriceNA)<-c("Nb.manquants","Nb.données","%")
      rownames(matriceNA)<-colnames(D)
      matriceNA
    },rownames=TRUE)
    
    
    ########################################################################################################################
    ####    OUTPUT page 3 : Descriptifs univaries          
    ########################################################################################################################
    
    output$propositions <- renderUI({
      
      selectInput("variable", "Variable:",   choices=noms()) 
    })
    
    # output$summary <- renderPrint({
    #   summary(BDD())
    # })
    
    output$descriptifUni <- renderText(paste("Descriptif de la variable ",input$variable, sep = ""))
    output$descvar <- renderTable({
      base    <-BDD()
      variable<-base[,colnames(base)==input$variable]
      print(input$variable)
      if(input$qualiquanti=="quant"){res<-data.frame(descr1(variable)$Descriptif)
      colnames(res) <- c("Descriptif")}
      if(input$qualiquanti=="qual") {res<-data.frame(desql(variable))
      colnames(res) <- c("Effectifs", "Proportions")}
      xtable(res, "essai")
    },hover = T,rownames=TRUE)
    
    output$plot1 <- renderPlot({
      base    <-BDD()
      variable<-base[,input$variable]
      if(input$qualiquanti=="quant"){
        print(   hist(variable,
                      xlab = input$variable,
                      ylab = "Effectif",
                      main= "Histogramme",
                      col = "#75AADB", border = "white") )
        
        
      }
      if(input$qualiquanti=="qual") {variable<-as.character(variable);print( diagrammeBarre(variable)  )}
    })
    
    output$plot2 <- renderPlot({
      base    <-BDD()
      variable<-base[,colnames(base)==input$variable]
      if(input$qualiquanti=="quant"){boxplot(x=variable,main="Diagramme boite", xlab = input$variable)}
      if(input$qualiquanti=="qual") {print(pieChart(variable))}
    })
    
    # 
    # quali
    # 
 
  })
}
    
## To be copied in the UI
# mod_Descriptifs_ui("Descriptifs_1")
    
## To be copied in the server
# mod_Descriptifs_server("Descriptifs_1")
