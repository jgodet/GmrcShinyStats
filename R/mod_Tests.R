#' Tests UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import pROC

mod_Tests_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
  uiOutput(ns('testsDiagnostiques'))
}
    
#' Tests Server Functions
#'
#' @noRd 
mod_Tests_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    source("./CodeSansDependance.R", local = TRUE)
    source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    eval(parse("./miseEnForme.R", encoding="UTF-8"))
    
    observe({
    output$testsDiagnostiques = renderUI({
      if(!r$BASEchargee) do.call(tabPanel,pasDeBase)
      else do.call(tabPanel,testsDiagnostiques)
    })
    })
    
    observeEvent(r$BDD,ignoreInit = T,{
      
    output$propositionsLOGIT1 <- renderUI({
      
      selectInput(ns("variableLogit1"), "Variable d'intérêt 0/1",   choices=r$noms) 
    })
    
    output$propositionsLOGIT2 <- renderUI({
      
      selectInput(ns("variableLogit2"), "Variable quantitative explicative",   choices=r$noms) 
    })
    
    output$mytableLOGIT1 <- renderTable({

      base<-r$BDD
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]
      data.frame(Reponse=variablesurvie1, Facteur=variablesurvie2)
      },rownames=TRUE)
#     
# 
# 
# 
# 
output$LogitROC <- renderPlot({
  base    <-r$BDD
  D       <-base

  #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
  variablesurvie1 <-base %>% select(input$variableLogit1)
  variablesurvie1 <- variablesurvie1[[1]]
  #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
  variablesurvie2 <-base %>% select(input$variableLogit2)
  variablesurvie2 <- variablesurvie2[[1]]


  rocobj<-plot.roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)


  if(input$LOGIToptionsSEUIL){
    optimums       <-ci(rocobj, of="thresholds", thresholds="best")
    plot(optimums) }

  if(input$LOGIToptionsIntervalle){
    ciobj           <- ci.se(rocobj, specificities=seq(0, 100, 5))
    plot(ciobj, type="shape", col="#1c61b6AA") }
})
})
 
 
# ## Creation des perf
    observeEvent(input$variableLogit1,ignoreInit = T,{

      base    <-r$BDD

      D       <-base
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]

print("variablesurvie1")
print(variablesurvie1)
print("variablesurvie2")
print(variablesurvie2)

  rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
  x<-ci.thresholds(rocobj)

  MatriceSEUILS<-cbind(
    attr(x, "thresholds"),
    x$ sensitivity,
    x$ specificity,
    x$ sensitivity[,2]+x$ specificity[,2]-100)
  colnames(MatriceSEUILS)<-c("Seuils","2.5% Sensibilité","Sensibilité","97.5% Sensibilité","2.5% Spécificité","Spécificité","97.5% Spécificité","Indice de Youden")
  r$MatriceReactiveSeuils<-MatriceSEUILS

})

observe({


    output$LogitROCtableau <- renderTable({

      base    <-r$BDD

      D       <-base
            #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]

      rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
      x<-ci.thresholds(rocobj)

      MatriceSEUILS<-cbind(
        attr(x, "thresholds"),
        x$ sensitivity,
        x$ specificity,
        x$ sensitivity[,2]+x$ specificity[,2]-100)
      colnames(MatriceSEUILS)<-c("Seuils","2.5% Sensibilité","Sensibilité","97.5% Sensibilité","2.5% Spécificité","Spécificité","97.5% Spécificité","Indice de Youden")
      MatriceSEUILS
    },rownames=TRUE)


    output$LogitROCtableauBEST <- renderTable({
      base    <-r$BDD

      D       <-base
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]

      rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
      MatriceSEUILS<-r$MatriceReactiveSeuils
      #x<-ci.thresholds(rocobj)

      #MatriceSEUILS<-cbind(
      #  attr(x, "thresholds"),
      #  x$ sensitivity,
      #  x$ specificity,
      #  x$ sensitivity[,2]+x$ specificity[,2]-100)
      #colnames(MatriceSEUILS)<-c("Seuils","2.5% Se","Se","97.5% Se","2.5% Sp","Sp","97.5% Sp","Indice de Youden")

      t(MatriceSEUILS[which(MatriceSEUILS[,8]==max(MatriceSEUILS[,8])),])
    },rownames=TRUE)




    output$LogitPERFtableauBEST <- renderTable({
      base    <-r$BDD

      D       <-base
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]

      rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
      x<-ci.thresholds(rocobj)

      MatriceSEUILS<-cbind(
        attr(x, "thresholds"),
        x$ sensitivity,
        x$ specificity,
        x$ sensitivity[,2]+x$ specificity[,2]-100)
      colnames(MatriceSEUILS)<-c("Seuils","2.5% Se","Se","97.5% Se","2.5% Sp","Sp","97.5% Sp","Indice de Youden")
      t(MatriceSEUILS[which(MatriceSEUILS[,8]==max(MatriceSEUILS[,8])),1])
    },rownames=TRUE)


    output$LogitPERF1 <- renderTable({
      base    <-r$BDD

      D       <-base
     #y <-base[,colnames(base)==input$variableLogit1]
     y<-base %>% select(input$variableLogit1)
     y<-y[[1]]
     #x <-base[,colnames(base)==input$variableLogit2]
     x<-base %>% select(input$variableLogit2)
     x<-x[[1]]

      rocobj                  <-roc(y,x,main=titre, percent=TRUE,ci=TRUE,print.auc=TRUE)
      optimums                <-ci(rocobj, of="thresholds", thresholds="best")

      AUC                     <-c( round(rocobj$ci[1],2),  round(rocobj$ci[2],2),  round(rocobj$ci[3],2) )
      best.cut                <-as.numeric(rownames(round(optimums$sensitivity,2)))
      best.sen                <-c(round(optimums$sensitivity,2))
      best.spe                <-c(round(optimums$specificity,2))

      SeSp                    <-rbind(best.sen,best.spe,AUC)
      rownames(SeSp)  =c("Sensibilité","Spécificité","AUC (Aire sous la courbe)")
      colnames(SeSp)  =c("2.5%","Val","97.5%")
      SeSp
    },rownames=TRUE)



    output$LogitPERF2 <- renderTable({
      base<-r$BDD

      D<-base
      #y <-base[,colnames(base)==input$variableLogit1]
      y<-base %>% select(input$variableLogit1)
      y<-y[[1]]
      #x <-base[,colnames(base)==input$variableLogit2]
      x<-base %>% select(input$variableLogit2)
      x<-x[[1]]
      
      rocobj<-roc(y,x,main=titre, percent=TRUE,ci=TRUE,print.auc=TRUE)
      optimums<-ci(rocobj, of="thresholds", thresholds="best")

      best.cut<-as.numeric(rownames(round(optimums$sensitivity,2)))

      y2 <-y
      x2 <-ifelse(x>best.cut,1,0)
      T <-table(x2,y2)
      VP<-T[2,2]
      VN<-T[1,1]
      FP<-T[2,1]
      FN<-T[1,2]
      V1<-cbind(VP,VN,FP,FN);colnames(V1)=c("VP","VN","FP","FN")

      VPP<-round(VP/(VP+FP),2)
      VPN<-round(VN/(VN+FN),2)
      Exact<-round((VP+VN)/(VP+VN+FP+FN),2)
      Erreur<-round((FP+FN)/(VP+VN+FP+FN),2)
      V2<-cbind(VPP,VPN,Exact,Erreur);colnames(V2)=c("VPP","VPN","Exactitude","Taux d'erreur")

      cbind(V1,V2)
    },rownames=TRUE)


    output$LogitPERF3 <- renderTable({
      base    <-r$BDD

      D <-base
      #y <-base[,colnames(base)==input$variableLogit1]
      y<-base %>% select(input$variableLogit1)
      y<-y[[1]]
      #x <-base[,colnames(base)==input$variableLogit2]
      x<-base %>% select(input$variableLogit2)
      x<-x[[1]]
      
      rocobj<-roc(y,x,main=titre, percent=TRUE,ci=TRUE,print.auc=TRUE)
      optimums<-ci(rocobj, of="thresholds", thresholds="best")

      best.cut<-as.numeric(rownames(round(optimums$sensitivity,2)))

      y2<-y
      x2<-ifelse(x>best.cut,1,0)
      T<-table(x2,y2)
      rownames(T)<-c("x<cut","x>cut")
      colnames(T)<-c("0","1")
      T
    },rownames=TRUE)
  })
  })
  
}
    
## To be copied in the UI
# mod_Tests_ui("Tests_1")
    
## To be copied in the server
# mod_Tests_server("Tests_1")
