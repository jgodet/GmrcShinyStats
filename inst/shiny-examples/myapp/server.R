

########################################################################################################################
####    FONCTIONS UTILES
########################################################################################################################

#source("./R/CodeSansDependance.R")
#source("./R/fonction.r")
#source("./R/miseEnForme.R")


# list.of.packages <- c("aa","shiny","ggplot2", "shinyFiles","dplyr","pROC","irr","moments","DT","gdata","stringr","boot","xtable")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#
# if(length(new.packages)>0){
#   install.packages(new.packages)
#   }

#library(markdown)
if(!require(ggplot2)){install.packages('ggplot2')}; library(ggplot2)
if(!require(shinyFiles)){install.packages('shinyFiles')}; library(shinyFiles)
if(!require(shiny)){install.packages('shiny')}; library(shiny)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(pROC)){install.packages('pROC')}; library(pROC)
if(!require(irr)){install.packages('irr')}; library(irr)
if(!require(moments)){install.packages('moments')}; library(moments)
if(!require(DT)){install.packages('DT')}; library(DT)
if(!require(gdata)){install.packages('gdata')}; library(gdata )
if(!require(stringr)){install.packages('stringr')}; library(stringr )
if(!require(boot)){install.packages('boot')}; library(boot )
if(!require(xtable)){install.packages('xtable')}; library(xtable )
if(!require(devtools)){install.packages('devtools')}; library(devtools)
if(!require(desctable)){install.packages('desctable')}
if(!require(gmrcfun)){install_github(repo = "jgodet/gmrcfun")}; library(gmrcfun)

server <- shinyServer(function(input, output, session) {
  #session$onSessionEnded(stopApp)
  #session$onSessionEnded(function() {

  # stopApp()
  #})





  ########################################################################################################################
  ####    LECTURE DE TOUS LES PDF
  ########################################################################################################################

  # Lecture de tous les PDF d'aide utilisateur

  output$formatBASE = downloadHandler(
    filename    = '0_Instructions.pdf',
    content     = function(file) file.copy('0_Instructions.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFbase = downloadHandler(
    filename    = '1_BaseDeDonnees.pdf',
    content     = function(file) file.copy('1_BaseDeDonnees.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFdescriptif1o1 = downloadHandler(
    filename    = '2_Descriptif.pdf',
    content     = function(file) file.copy('2_Descriptif.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFdescriptif1o2 = downloadHandler(
    filename    = '2_Descriptif.pdf',
    content     = function(file) file.copy('2_Descriptif.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )
  output$PDFdescriptif1o3 = downloadHandler(
    filename    = '2_Descriptif.pdf',
    content     = function(file) file.copy('2_Descriptif.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFdescriptif2 = downloadHandler(
    filename    = '2_DescriptifVAR.pdf',
    content     = function(file) file.copy('2_DescriptifVAR.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFcroisements = downloadHandler(
    filename    = '3_Croisements.pdf',
    content     = function(file) file.copy('3_Croisements.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFsurvie = downloadHandler(
    filename    = '4_Survie.pdf',
    content     = function(file) file.copy('4_Survie.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFdiag = downloadHandler(
    filename    = '5_Diagnostiques.pdf',
    content     = function(file) file.copy('5_Diagnostiques.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$PDFconcordance = downloadHandler(
    filename    = '6_Concordance.pdf',
    content     = function(file) file.copy('6_Concordance.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  output$DLcnil = downloadHandler(
    filename    = 'DBnonCRIH.pdf',
    content     = function(file) file.copy('DBnonCRIH.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

  # Est-ce que la base de données est chargée ?

  ########################################################################################################################
  ####    OUTPUT page 0 : Accueil
  ########################################################################################################################

  # télécharger une exemple de fichier Excel, base de données type
  output$DLcsv <- downloadHandler(
    filename ='ExempleCSV.csv',
    content = function(file) file.copy('ExempleCSV.csv', file, overwrite = TRUE),
    contentType = 'application/csv'

  )

  ############## Filtre ##############
  # output$filtre = renderUI({
  #   if(FILTREapplique()) tags$code("Un filtre est actuellement appliqué à la base de données")
  #   else(ez)
  #

  #
  # })

  ########################################################################################################################
  ####    OUTPUT page 1 : Lecture de la base de donnees
  ########################################################################################################################


  #   NOUVELLE FACON DE CHARGER UNE BASE DE DONNEES
  # observe({
  #
  #   if (input$browse == 0)    return()
  #
  #
  #   updateTextInput(session, "path",  value = file.choose2())
  # })
  #
  volumes = getVolumes()()
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

      D<-read.csv(paste(as.character(file_selected$datapath), collapse = "\n"), header=input$header,sep=input$sep, na.string=c("",input$manquants),dec=input$decimale, fileEncoding = input$encodage,stringsAsFactors = T)
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

  # output$FILTREapplique<- renderText({
  #   if(!is.null(input$file1)){
  #   inFile  <- input$file1
  #   D<-read.csv(inFile$datapath, header=input$header, sep=input$sep, na.string=c("",input$manquants),dec=input$decimale)
  #   FILTRE<-ifelse(dim(D[])[1]==dim(D)[1],"0","1")
  # FILTRE}else{"0"}
  # })



  ########################################################################################################################
  ####    OUTPUT page 3 : Descriptif de la base et NA
  ########################################################################################################################

  output$univarie = renderUI({
    if(!BASEchargee()){
      do.call(tabPanel,pasDeBase())
    }else{
    #source("./univarie.r")
      do.call(tabPanel,univarie())
    }
  })


  output$plotNAbase1 <- renderPlot({
    plot_na(BDD())
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



  #######################################################################
  #####   PAGE 4 CROISEMENTS INFERENCE   ###############################
  #######################################################################
  output$croisementsInference = renderUI({
    if(!BASEchargee()){
      do.call(tabPanel,pasDeBase())
    }else{
      #source("./croisementsInference.r")
      do.call(tabPanel,croisementsInference())
    }
  })



  output$propositionsCROISE1 <- renderUI({

    selectInput("variableCROISE1", "Variable:",   choices=noms())
  })

  output$propositionsCROISE2 <- renderUI({

    selectInput("variableCROISE2", "Variable:",   choices=noms())
  })



  output$plotCROISE <- renderPlot({
    base    <-BDD()
    variableCROISE1 <-base[,input$variableCROISE1]
    variableCROISE2 <-base[,input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="quant"){print(ggpoints(variableCROISE1,variableCROISE2,nomx =input$variableCROISE1, nomy = input$variableCROISE2 ))}
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){boxplot(variableCROISE1~variableCROISE2, xlab=input$variableCROISE2, ylab=input$variableCROISE1)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){boxplot(variableCROISE2~variableCROISE1,xlab=input$variableCROISE1, ylab=input$variableCROISE2)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="qual"){

      # print(ggpie(as.factor(variableCROISE1),as.factor(variableCROISE2)))
      BDD<- base[,c(input$variableCROISE1,input$variableCROISE2)]
      pourcent <-  prop.table(table(BDD),1)
      data<-as.data.frame(pourcent)
      maxPourcent<- max(data$Freq, na.rm = T)
      label<-  paste(round(data$Freq,3)*100,"%")
      vjust<- unlist(as.list(ifelse(data$Freq< maxPourcent/5, -1.6, 1.6)), use.names = F)

      barplotCroise <- ggplot(data=data, aes(x=rep(levels(data[,2]),length(levels(data[,1]))) ,y=Freq))+
        geom_bar(stat="identity", position = "dodge",color='black',aes(fill = rep(levels(data[,2]),length(levels(data[,1])))))+
        facet_wrap(formule(input$variableCROISE1))+
        geom_text(data=data,aes( label = paste(round(Freq,3)*100,"%")) , vjust=vjust, color="black", size=5) +
        theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=17))+
        ggtitle(paste("En fonction de ", input$variableCROISE1, sep = ""))+
        xlab(input$variableCROISE2)+
        labs(fill = input$variableCROISE2)

      print(barplotCroise)


    }
  })
  output$plotCROISE2 <- renderPlot({
    base    <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="quant"){    print(correl(variableCROISE1,variableCROISE2, nomx=input$variableCROISE1 , nomy= input$variableCROISE2))}
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){     print(ggcompar(input$variableCROISE1,input$variableCROISE2,base))}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){     print(ggcompar(input$variableCROISE2,input$variableCROISE1,base))}
  })




  output$montableauCroisAUTO <- renderText({

    base    <-BDD()
    x <-base[,input$variableCROISE1]
    y <-base[,input$variableCROISE2]
    Matrice    <-  addmargins(table(x,y, dnn = c(input$variableCROISE1,input$variableCROISE2), useNA =  input$NATableau))
    colnames(Matrice)<- ifelse(is.na(colnames(Matrice)), "Données Manquantes",colnames(Matrice))
    rownames(Matrice)<- ifelse(is.na(rownames(Matrice)), "Données Manquantes",rownames(Matrice))
    var2<-input$variableCROISE2
    var1<-input$variableCROISE1
    entete<-paste("<tr><th></th><th colspan='",dim(Matrice)[2],"'>",var2 ,"</td></tr>
                  <tr> <th>",var1 ," </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>",x, "</th>",sep = "") )),collapse=""),"</tr>",sep="")


    print.xtable(xtable( Matrice,caption = "Effectifs"),
                 size="footnotesize", #Change size; useful for bigger tables
                 include.rownames=T,
                 caption.placement="top",
                 hline.after=NULL,
                 include.colnames=FALSE,
                 add.to.row = list(pos = list(0),
                                   command=entete), type = "html",  html.table.attributes='class = "pure-table"  ',
                 print.results =F)
  })

  output$montableauCroise2AUTO <- renderText({
    base    <-BDD()
    x <-base[,colnames(base)==input$variableCROISE1]
    y <-base[,colnames(base)==input$variableCROISE2]
    Matrice    <- addmargins(100 * prop.table(addmargins(table(x,y, useNA = input$NATableau), 1), 1), 2)
    colnames(Matrice)<- ifelse(is.na(colnames(Matrice)), "Données Manquantes",colnames(Matrice))
    rownames(Matrice)<- ifelse(is.na(rownames(Matrice)), "Données Manquantes",rownames(Matrice))
    var2<-input$variableCROISE2
    var1<-input$variableCROISE1
    entete<-paste("<tr><th></th><th colspan='",dim(Matrice)[2],"'>",var2 ,"</td></tr>
                  <tr> <th>",var1 ," </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>",x, "</th>",sep = "") )),collapse=""),"</tr>",sep="")


    print.xtable(xtable( Matrice,caption = "Pourcentages ligne", digits = 2),
                 size="footnotesize", #Change size; useful for bigger tables
                 include.rownames=T,
                 caption.placement="top",
                 hline.after=NULL,
                 include.colnames=FALSE,
                 add.to.row = list(pos = list(0),
                                   command=entete), type = "html",  html.table.attributes='class = "pure-table"  ',
                 print.results =F)


  })

  output$montableauCroise3AUTO <-renderText({
    base    <-BDD()
    x <-base[,colnames(base)==input$variableCROISE1]
    y <-base[,colnames(base)==input$variableCROISE2]
    Matrice    <- addmargins(100 * prop.table(addmargins(table(x,y, useNA = input$NATableau), 2), 2), 1)
    colnames(Matrice)<- ifelse(is.na(colnames(Matrice)), "Données Manquantes",colnames(Matrice))
    rownames(Matrice)<- ifelse(is.na(rownames(Matrice)), "Données Manquantes",rownames(Matrice))
    var2<-input$variableCROISE2
    var1<-input$variableCROISE1
    entete<-paste("<tr><th></th><th colspan='",dim(Matrice)[2],"'>",var2 ,"</td></tr>
                  <tr> <th>",var1 ," </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>",x, "</th>",sep = "") )),collapse=""),"</tr>",sep="")


    print.xtable(xtable( Matrice,caption = "Pourcentages colonne", digits = 2),
                 size="footnotesize", #Change size; useful for bigger tables
                 include.rownames=T,
                 caption.placement="top",
                 hline.after=NULL,
                 include.colnames=FALSE,
                 add.to.row = list(pos = list(0),
                                   command=entete), type = "html",  html.table.attributes='class = "pure-table"  ',
                 print.results =F)





  })


  output$AUTOtableCHI2 <- shiny::renderTable({
    base    <-BDD()
    x <-base[,colnames(base)==input$variableCROISE1]
    y <-base[,colnames(base)==input$variableCROISE2]
    Mat    <- table(x,y)
    CH2<-stats::chisq.test(Mat,correct=FALSE)
    resTESTS<-cbind(CH2$statistic,CH2$parameter,CH2$ p.value)
    colnames(resTESTS)<-c("CHI2 Stat","CHI2 Degrés","CHI2 pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE,colnames=TRUE)

  output$AUTOtableFISHER <- shiny::renderTable({
    base    <-BDD()
    x <-base[,colnames(base)==input$variableCROISE1]
    y <-base[,colnames(base)==input$variableCROISE2]
    Mat    <- table(x,y)
    FI2<-stats::fisher.test(Mat)
    resTESTS<-t(t( FI2$ p.value))
    colnames(resTESTS)<-c("Fisher pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)


  output$AUTOCHI2conditions <- renderText({
    base    <-BDD()
    x <-base[,colnames(base)==input$variableCROISE1]
    y <-base[,colnames(base)==input$variableCROISE2]
    Mat    <- table(x,y)
    CH2<-stats::chisq.test(Mat,correct=FALSE)
    FI2<-stats::fisher.test(Mat)
    ifelse(all(CH2$expected>5),"Au vu des effectifs théoriques >5, on préfèrera ici l'utilisation du test du Chi2",
           "Au vu des faibles effectifs théoriques, on préfèrera ici l'utilisation du test exact de Fisher")
  })

  output$oddratioAUTO <- renderTable({
    base    <-BDD()
    x <-base[,colnames(base)==input$variableCROISE1]
    y <-base[,colnames(base)==input$variableCROISE2]
    Mat    <- table(x,y)

    Nblignes   <-dim(Mat)[1]
    Nbcolonnes <-dim(Mat)[2]
    if(Nblignes>2 | Nbcolonnes>2){OR<-NULL}else{
      FI2<-stats::fisher.test(Mat)
      OR<-cbind(FI2$ estimate , FI2$ conf.int[[1]],FI2$ conf.int[[2]])
      colnames(OR)<-c("Rapport de cotes","Borne inf 2.5","Borne Sup 97.5")
      rownames(OR)<-"Résultat"}
    OR
  }, caption = "Rapport de cotes et IC",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)


  output$descr3DESCRIPTIF<- renderTable({
    base    <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2,nom = input$variableCROISE2, nomY = input$variableCROISE1)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1,nom = input$variableCROISE1, nomY = input$variableCROISE2)}
    res$Descriptif
  }, caption = "Descriptif global et par modalité",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

  output$descr3TestNormalite<- renderPrint({
    base    <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
    print(res[2])
  })
  output$descr3Testpv<- renderPrint({
    base    <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
    print(res[3])
  })
  output$descr3TestsNPv<- renderPrint({
    base    <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
    print(res[4])
  })

  output$descr3Tests_de_Student<- renderPrint({
    base    <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
    print(res[5])
  })


  output$descr3TestsMANN<- renderPrint({
    base    <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
    print(res[6])
  })



  output$CorrelationCROISE<- renderPrint({
    base            <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
    x<-variableCROISE1
    y<-variableCROISE2
    resultatCorrelation<-
      cat("Coefficient de corrélation de Pearson\n Le coefficient de corrélation linéaire de Pearson (Rho) est estimé à",
          round(cor.test(x,y,method = "pearson" )$estimate,3),
          "et son intervalle de confiance à 95% est: [",
          round(cor.test(x,y)$ conf.int[1],3),";",round(cor.test(x,y)$ conf.int[2],3),
          "] \n La p.valeur associée au test de nullité de ce coefficient est estimée à:",
          round(cor.test(x,y,method = "pearson") $p.value,4),"\n\n\n\n",
          "Coefficient de corrélation de Spearman\n Le coefficient de corrélation non-paramétrique de Spearman est estimé à:",
          round(cor.test(x,y,method="s")$estimate,3),
          " \n La p.valeur associée au test de nullité de ce coefficient est estimée à:",
          round(cor.test(x,y,method = "s") $p.value,4),"\n\n",
          "\n\n\n Il n'est pas préconisé d'utiliser un test plutôt qu'un autre. Le test de Pearson est un test de corrélation linéaire entre les deux mesures. Le test de Spearman est un test non-paramétrique, plutôt adapté à des relations non-linéaires entre les variables.
      \n\n Le graphique ci-dessous représente l'éventuelle corrélation de ces deux variables et l'intervalle de confiance qui lui est associé."
      )

  })

  output$ChoixSortieCROISE<- renderPrint({
    base            <-BDD()
    variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
    variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]


    ChoixTest<-function(Y,X){

      nom<-deparse(substitute(X))
      if(!is.factor(X)){X<-as.factor(X)}
      nomY<-deparse(substitute(Y))
      nbnv<-nlevels(X)
      library(moments)

      if(nlevels(X)==2){pvaleur<-format.pval(wilcox.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvaleur<-format.pval(kruskal.test(Y~X)$p.value,digits=4)}}

      if(nlevels(X)==2){testnp<-paste("Test de Mann & Whitney : p =",pvaleur)}else{if(nlevels(X)>2){testnp<-paste("Test de Kruskal & Wallis : p =",pvaleur)}}

      pvalstud<-matrix(c(NA,NA),ncol=1)
      rownames(pvalstud)<-c("Test de Student, variances egales   : p =","Test de Student, variances inegales : p =")
      colnames(pvalstud)<-c("")

      if(nlevels(X)==2)
      {pvalstud[1]<-t.test(Y~X,var.equal = TRUE)$p.value}
      else
      {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}

      if(nlevels(X)==2)
      {pvalstud[2]<-t.test(Y~X,var.equal = FALSE)$p.value}
      else
      {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}


      if(nlevels(X)==2){testp<-round(pvalstud,digits=4)}else{if(nlevels(X)>2){testp<-paste("Analyse de la Variance : p =",pval)}}

      if(nlevels(X)==2){pvartest<-format.pval(var.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvartestpg<-format.pval(bartlett.test(Y~X)$p.value,digits=4)}}

      if(nlevels(X)==2){testpv<-paste(list(paste("Test parametrique d'egalite de deux variances (Fisher): p =",pvartest)
      ))
      }else{if(nlevels(X)>2){testpv<-paste("Test parametrique d'egalite de plus de deux variances (Bartlett) : p =",pvartestpg)}}

      pvalnorm<-matrix(c(NA,NA),ncol=1)
      rownames(pvalnorm)<-c("Test de normalite de Shapiro-Wilk       : p =","Test de normalite de Kolmogorov-Smirnov : p =")
      colnames(pvalnorm)<-c("")
      if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value}else{pvalnorm[1]<-NA}
      pvalnorm[2]<-ks.test(Y,"pnorm",mean(Y,na.rm=T),sd(Y,na.rm=T))$p.value
      pvalnorm<-round(pvalnorm,digits=4)

      if(nlevels(X)==2){pvalfl2g<-format.pval(ansari.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvalfl3g<-format.pval(fligner.test(Y~X)$p.value,digits=4)}}

      if(nlevels(X)==2){testnpv<-paste(list(paste("Test non param. d'egalite de deux variances (Ansari) : p =",pvalfl2g)
      ))
      }else{if(nlevels(X)>2){testnpv<-paste("Test non param. d'egalite de plus de deux variances (Fligner) : p =",pvalfl3g)}}


      ############# le choix	#########
      if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value

      if(nlevels(X)==2){
        if(pvalnorm[1]>0.05){
          if(pvartest>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser un test de Student - variances égales"}else{choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser un test deStudent - variances inégales"}
        }else{
          if(pvalfl2g>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les distributions d'utiliser le test de Mann-Whitney"}else{choix<-"La distribution des données est irrégulière dans les groupes, aucun des tests ne permet ici une comparaison des groupes. Les hypothèses du test de Mann-Whitney ne sont pas vérifiées."}
        }# fin else shapiro
      }# fin levels =2

      if(nlevels(X)>2){
        if(pvalnorm[1]>0.05){
          if(pvartestpg>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser l'ANOVA moyennes égales"}else{choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser l'ANOVA moyennes inégales"}
        }else{
          if(pvalfl3g>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les distributions d'utiliser le test de Kruskal-Wallis"}else{choix<-"La distribution des données est irrégulière dans les groupes, aucun des tests ne permet ici une comparaison des groupes. Les hypothèses du test de Mann-Whitney ne sont pas vérifiées"}
        }# fin else shapiroe RIEN
      }# fin levels =2
      return(choix)
      }else{# fin si moins de 5000
        res<-"Les effectifs sont très importants, il est préconisé ici d'utiliser le test de Student pour comparer les moyennes."
      }
    }# fin function

    if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){
      res<-ChoixTest(variableCROISE1,variableCROISE2)
    }
    if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){
      res<-ChoixTest(variableCROISE2,variableCROISE1)
    }
    res
  })


  ####################### Tableau de croisement ###########################

  output$propositionsTableauCROISE <- renderUI({

    selectInput("VariableCroisement", "Variable de croisement:",   choices=noms()[nbModeVariable()<5])
  })

  #/home/tibo/Bureau/Shiny/BDD.csv
  # Variable Quanti "normale"

  listeVariableNormale <- reactive({
    if(sum( variableNormale(), na.rm = T)==0){
      listeVariableNormale <-  ""
    }else{ listeVariableNormale =c(noms()[which(nbModeVariable()>1 & variableNum() & variableNormale())],"")
    }
    listeVariableNormale
  })


  listeVariableNonNormale <- reactive({
    if(sum( !variableNormale(), na.rm = T)==0){
      listeVariableNonNormale <-  ""
    }else{ listeVariableNonNormale =c(noms()[which(nbModeVariable()>1 & variableNum() & !variableNormale())],"")
    }
    listeVariableNonNormale
  })

  output$selectionVariablesCroisees1 <- renderUI({

    selectInput("VariableCroisees1", "Variables Croisées Quantitatives (moyenne et écart-type)",
                choices =  list(`Variables Normales` =listeVariableNormale(),
                                `Variables non Normales` = listeVariableNonNormale())
                ,
                selected = NULL,
                multiple = TRUE    )
  })

  output$selectionVariablesCroisees3 <- renderUI({

    selectInput("VariableCroisees3", "Variables Croisées Quantitatives (médiane, 1er et 3ème quartiles)",
                choices =  list(`Variables non Normales` = listeVariableNonNormale(),
                                `Variables Normales` =listeVariableNormale())
                ,
                selected = NULL,
                multiple = TRUE    )
  })

  output$selectionVariablesCroisees2 <- renderUI({

    selectInput("VariableCroisees2", "Variables Croisées Qualitatives (pourcentage et effectif)",
                choices = noms()[nbModeVariable()<13& nbModeVariable()>1],
                selected = NULL,
                multiple = TRUE    )
  })


  tableCroise <- reactive({
    base<- BDD()
    if(is.null(input$VariableCroisees1)&is.null(input$VariableCroisees2) & is.null(input$VariableCroisees3)){
      # if(is.null(input$VariableCroisees1)){  tableauCroisement<- cbind( base%>% select(input$VariableCroisees2)%>% mutate_all(factor))
      # } else if(is.null(input$VariableCroisees2)){  tableauCroisement<- cbind(base%>% select(input$VariableCroisees1))
    }else{
      #
      #   variablecommune<- input$VariableCroisees2[input$VariableCroisees2%in% input$VariableCroisees1]
      #   if(!length(variablecommune)==0){
      #     variablecommune2<- paste(variablecommune,"qual")
      #     base[,variablecommune2]<- base[,variablecommune]
      #     variabletableau2<- c(input$VariableCroisees2[!input$VariableCroisees2%in% input$VariableCroisees1],variablecommune2)
      #   }else{
      #     variabletableau2<-input$VariableCroisees2
      #   }
      #   tableauCroisement<- cbind(base%>% select(input$VariableCroisees1), base%>% select(variabletableau2)%>% mutate_all(factor))
      # }

      #Création tableau de croisement des variables à distributions normales
      if(!is.null(input$VariableCroisees1)){
        tableauCroisement1 <-  cbind(base%>% select(input$VariableCroisees1))
        names(tableauCroisement1) <-paste( input$VariableCroisees1, "(moyenne (sd))")
        names(tableauCroisement1) <-paste(names(tableauCroisement1),"(Na=", apply(tableauCroisement1, 2,function(x) sum(is.na(x))),")", sep = "")

        tableauCroisement1<-tableauCroisement1 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
          desctable ::desctable(stats = list("mean_p" = is.factor ~ percent | mean,
                                             "sd" = is.factor ~ length | sd),tests =tests_autoGMRC ) %>%
          as.data.frame
        colonne<- (dim(tableauCroisement1)[2]-3)/2
        tableauCroisement1Sortie <- tableauCroisement1[, c(1, (c(1:colonne)* 2),dim(tableauCroisement1)[2]-1, dim(tableauCroisement1)[2]) ]
        for(i in 1 :colonne ){
          tableauCroisement1Sortie[,(1+i)] <- paste(round(tableauCroisement1[,(i*2)],input$nbDec)," (",round(tableauCroisement1[,(i*2+1)],input$nbDec),")", sep= "")
        }

        names(tableauCroisement1Sortie)[1]= "Variables"
      }else{tableauCroisement1Sortie<-NULL}



      if(!is.null(input$VariableCroisees2)){tableauCroisement2 <-  cbind(base%>% select(input$VariableCroisees2)%>% mutate_all(factor))
      # names(tableauCroisement2) <-paste( input$VariableCroisees2, "(pourcentage(nombre))")
      names(tableauCroisement2) <-paste(names(tableauCroisement2),"\n (Na=", apply(tableauCroisement2, 2,function(x) sum(is.na(x))),")", sep = "")

      tableauCroisement2<-tableauCroisement2 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
        desctable ::desctable(stats = list("mean_p" = is.factor ~ percent | mean,
                                           "sd" = is.factor ~ length | sd),tests =tests_autoGMRC ) %>%
        as.data.frame
      colonne<- (dim(tableauCroisement2)[2]-3)/2
      tableauCroisement2Sortie <- tableauCroisement2[, c(1, (c(1:colonne)* 2),dim(tableauCroisement2)[2]-1, dim(tableauCroisement2)[2]) ]
      for(i in 1 :colonne ){

        tableauCroisement2Sortie[,(1+i)] <- paste(round(tableauCroisement2[,(i*2)],input$nbDec),"% (",round(tableauCroisement2[,(i*2+1)],input$nbDec),")")
      }
      # aRajouter<-substr(tableauCroisement2Sortie[1,],regexpr(":",tableauCroisement2Sortie[1,])[1],nchar(tableauCroisement2Sortie[1,]))
      # aRajouter<- ""
      tableauCroisement2Sortie[,1] <-gsub(" Groupe","",tableauCroisement2Sortie[,1])
      tableauCroisement2Sortie[,1] <- paste(gsub(".*:","Groupe :",tableauCroisement2Sortie[,1]),"(pourcentage(effectif))" )
      tableauCroisement2Sortie[,1] <-gsub(") .*",")(effectif)",tableauCroisement2Sortie[,1])
      for( i in 2: (dim(tableauCroisement2Sortie)[2]-2)){
        tableauCroisement2Sortie[,i] <- gsub("NA %","",tableauCroisement2Sortie[,i])
      }
      names(tableauCroisement2Sortie)[1]= "Variables"
      }else{tableauCroisement2Sortie<-NULL}


      #Création tableau de croisement des variables à distributions normales mediane / 1er et 3eme quartiles
      if(!is.null(input$VariableCroisees3)){
        tableauCroisement3 <-  cbind(base%>% select(input$VariableCroisees3))
        names(tableauCroisement3) <-paste( input$VariableCroisees3, "(mediane(1er-3ème quartiles))")
        names(tableauCroisement3) <-paste(names(tableauCroisement3),"\n (Na=", apply(tableauCroisement3, 2,function(x) sum(is.na(x))),")", sep = "")
        tableauCroisement3<-tableauCroisement3 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
          desctable ::desctable(stats = list("mean_p" = median,
                                             "Q1" = function(x) round(quantile(x,0.25),input$nbDec), "Q3"= function(x) round(quantile(x,0.75),input$nbDec)),tests =tests_autoGMRC ) %>%
          as.data.frame
        colonne<- (dim(tableauCroisement3)[2]-3)/3
        tableauCroisement3Sortie <- tableauCroisement3[, c(1, (c(1:colonne)* 3-1),dim(tableauCroisement3)[2]-1, dim(tableauCroisement3)[2]) ]
        for(i in 1 :colonne ){
          tableauCroisement3Sortie[,(1+i)] <- paste(round(tableauCroisement3[,(i*3)-1],input$nbDec),"
                                                  (",tableauCroisement3[,(i*3)], "-", tableauCroisement3[,(i*3)+1],")", sep="")
        }

        names(tableauCroisement3Sortie)[1]= "Variables"

      }else{tableauCroisement3Sortie<-NULL}


      tableauCroisementSortie<- rbind(tableauCroisement1Sortie,tableauCroisement3Sortie,tableauCroisement2Sortie)

      nomColonne<-names(tableauCroisementSortie)

      nomColonne<- gsub("tests /","",nomColonne)
      nomColonne<- gsub("/ ","",nomColonne)
      nomColonne<- gsub("\\_p.*","",nomColonne)
      nomColonne<- gsub("sd.*","",nomColonne)
      nomColonne<- gsub("mean","",nomColonne)
      nomColonne<-gsub(".*:",paste(input$VariableCroisement,":"),nomColonne)
      # nomColonne<-gsub("\\base","",nomColonne)
      names(tableauCroisementSortie)<- nomColonne

      # tableauCroisement2<-tableauCroisement1 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
      #   desctable ::desctable(stats = list("mean_p" = is.factor ~ percent | mean,
      #                                      "sd" = is.factor ~ length | sd),tests =tests_autoGMRC ) %>%
      #
      #   as.data.frame
      # names(tableauCroisemen2)[1]= "Variables"
      #
      colp<-dim(tableauCroisementSortie)[2]-1
      tableauCroisementSortie[, colp]<-ifelse( tableauCroisementSortie[, colp]<1/10^(input$nbDec),paste("<",as.character(1/10^(input$nbDec)),sep=""), round(tableauCroisementSortie[, colp],input$nbDec))
      tableauCroisementSortie[,colp+1]<- gsub("%>%","",tableauCroisementSortie[,colp+1])
      if (input$tableauCroiseSimpli ==1){
        tableauCroisementSortie[,1]<- gsub("\\(moyenne \\(sd\\)\\)","*",tableauCroisementSortie[,1])
        tableauCroisementSortie[,1]<- gsub("\\(mediane\\(1er-3ème quartiles\\)\\)","°",tableauCroisementSortie[,1])
        tableauCroisementSortie[,1]<- gsub("\\(pourcentage\\(effectif\\)\\)","§",tableauCroisementSortie[,1])
        tableauCroisementSortie[,1]<- gsub("\\(effectif\\)","§",tableauCroisementSortie[,1])


      }
      tableauCroisementSortie


    }
  })



  output$downloadData <- downloadHandler(
    filename ="Tableau Croisement.csv",



    content = function(file) {



      write.csv2(tableCroise(), file)
    }
  )

  output$tableauCroisement <- renderTable({
    tableCroise()


  }, na = "", digits = 3)

  output$legende <- renderUI({

    if (input$tableauCroiseSimpli ==1){
      affichage<-HTML("* moyenne (sd)
          <br/>° mediane (1er-3ème quartile)
          <br/>§ pourcentage (effectif)")
      affichage
    }

  })


  ########################################################################################################################
  ####    OUTPUT page 5 : Analyse de survie      #################################################
  ########################################################################################################################

  output$analyseDeSurvie = renderUI({
    if(!BASEchargee()){
      do.call(tabPanel,pasDeBase())
    }else{
      #source("./analyseDeSurvie.r")
      do.call(tabPanel,analyseDeSurvie())
    }
  })


  output$propositionsSURVIE1 <- renderUI({

    selectInput("variablesurvie1", "Variable délai",   choices=noms())
  })

  output$propositionsSURVIE2 <- renderUI({

    selectInput("variablesurvie2", "Variable évenement 0/1:",   choices=noms()[nbModeVariable()==2])
  })

  output$propositionsSURVIE3 <- renderUI({
    noms2    <-noms()[nbModeVariable()<30]
    selectInput("variablesurvie3", "Variable groupe",   choices=noms2)
  })

  output$plotSURVIE <- renderPlot({
    base    <-BDD()

    variablesurvie1 <-base[,colnames(base)==input$variablesurvie1]
    variablesurvie2 <-base[,colnames(base)==input$variablesurvie2]
    variablesurvie3 <-base[,colnames(base)==input$variablesurvie3]
    if(!input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2      ) }
    if( input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2,variablesurvie3 ) }
  })


  output$sortieSURVIE2<- renderPrint({
    base    <-BDD()
    variablesurvie1 <-base[,colnames(base)==input$variablesurvie1]
    variablesurvie2 <-base[,colnames(base)==input$variablesurvie2]
    variablesurvie3 <-base[,colnames(base)==input$variablesurvie3]
    if(!input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2      ) }
    if( input$SURVIEcompar){    ggsurvie(variablesurvie1,variablesurvie2,variablesurvie3 ) }
  })





  ########################################################################################################################
  ####    OUTPUT page LOGIT    ############################
  ########################################################################################################################

  output$testsDiagnostiques = renderUI({
    if(!BASEchargee()){
      do.call(tabPanel,pasDeBase())
    }else{
      #source("./testsDiagnostiques.r")
      do.call(tabPanel,testsDiagnostiques())
    }


  })

  output$propositionsLOGIT1 <- renderUI({

    selectInput("variableLogit1", "Variable d'intérêt 0/1",   choices=noms())
  })

  output$propositionsLOGIT2 <- renderUI({

    selectInput("variableLogit2", "Variable quantitative explicative",   choices=noms())
  })

  output$mytableLOGIT1 <- renderTable({

    base<-BDD()
    variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
    variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
    data.frame(Reponse=variablesurvie1, Facteur=variablesurvie2)
  },rownames=TRUE)





  output$LogitROC <- renderPlot({
    base    <-BDD()

    D       <-base
    variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
    variablesurvie2 <-base[,colnames(base)==input$variableLogit2]

    rocobj<-plot.roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)


    if(input$LOGIToptionsSEUIL){
      optimums       <-ci(rocobj, of="thresholds", thresholds="best")
      plot(optimums) }

    if(input$LOGIToptionsIntervalle){
      ciobj           <- ci.se(rocobj, specificities=seq(0, 100, 5))
      plot(ciobj, type="shape", col="#1c61b6AA") }
  })


  ## Creation des perf
  MatriceReactiveSeuils      <- reactive({

    base    <-BDD()

    D       <-base
    variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
    variablesurvie2 <-base[,colnames(base)==input$variableLogit2]

    rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
    x<-ci.thresholds(rocobj)

    MatriceSEUILS<-cbind(
      attr(x, "thresholds"),
      x$ sensitivity,
      x$ specificity,
      x$ sensitivity[,2]+x$ specificity[,2]-100)
    colnames(MatriceSEUILS)<-c("Seuils","2.5% Sensibilité","Sensibilité","97.5% Sensibilité","2.5% Spécificité","Spécificité","97.5% Spécificité","Indice de Youden")
    MatriceSEUILS

  })


  output$LogitROCtableau <- renderTable({

    base    <-BDD()

    D       <-base
    variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
    variablesurvie2 <-base[,colnames(base)==input$variableLogit2]

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
    base    <-BDD()

    D       <-base
    variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
    variablesurvie2 <-base[,colnames(base)==input$variableLogit2]

    rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
    MatriceSEUILS<-MatriceReactiveSeuils()
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
    base    <-BDD()

    D       <-base
    variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
    variablesurvie2 <-base[,colnames(base)==input$variableLogit2]

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
    base    <-BDD()

    D       <-base
    y <-base[,colnames(base)==input$variableLogit1]
    x <-base[,colnames(base)==input$variableLogit2]

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
    base    <-BDD()

    D       <-base
    y <-base[,colnames(base)==input$variableLogit1]
    x <-base[,colnames(base)==input$variableLogit2]
    rocobj                  <-roc(y,x,main=titre, percent=TRUE,ci=TRUE,print.auc=TRUE)
    optimums                <-ci(rocobj, of="thresholds", thresholds="best")

    best.cut                <-as.numeric(rownames(round(optimums$sensitivity,2)))

    y2                              <-y
    x2                              <-ifelse(x>best.cut,1,0)
    T                               <-table(x2,y2)
    VP                              <-T[2,2]
    VN                              <-T[1,1]
    FP                              <-T[2,1]
    FN                              <-T[1,2]
    V1                              <-cbind(VP,VN,FP,FN);colnames(V1)=c("VP","VN","FP","FN")

    VPP                             <-round(VP/(VP+FP),2)
    VPN                             <-round(VN/(VN+FN),2)
    Exact                   <-round((VP+VN)/(VP+VN+FP+FN),2)
    Erreur                  <-round((FP+FN)/(VP+VN+FP+FN),2)
    V2                              <-cbind(VPP,VPN,Exact,Erreur);colnames(V2)=c("VPP","VPN","Exactitude","Taux d'erreur")

    cbind(V1,V2)
  },rownames=TRUE)


  output$LogitPERF3 <- renderTable({
    base    <-BDD()

    D       <-base
    y <-base[,colnames(base)==input$variableLogit1]
    x <-base[,colnames(base)==input$variableLogit2]
    rocobj                  <-roc(y,x,main=titre, percent=TRUE,ci=TRUE,print.auc=TRUE)
    optimums                <-ci(rocobj, of="thresholds", thresholds="best")

    best.cut                <-as.numeric(rownames(round(optimums$sensitivity,2)))

    y2                              <-as.factor(y)
    x2                              <-as.factor(ifelse(x>best.cut,1,0))
    T                               <-table(x2,y2)
    T <- rbind(T[1,], T[2,])
    rownames(T)<-c("x<cut","x>cut")
    colnames(T)<-c("0","1")
    T
  },rownames=TRUE)







  ########################################################################################################################
  ####    OUTPUT page 7 CONCORDANCE
  ########################################################################################################################


  output$concordance = renderUI({
    if(!BASEchargee()){
      #source("./concordanceSansBase.r")
      do.call(tabPanel,concordanceSansBase())
    }else{
      #source("./concordanceAvecBase.r")
      do.call(tabPanel,concordanceAvecBase())
    }
  })

  output$CONCORDANCElecture1 <- renderUI({

    selectInput("CONCORDANCElecture1", "Variable qualitative: Lecteur 1",   choices=noms())
  })

  output$CONCORDANCElecture2 <- renderUI({

    selectInput("CONCORDANCElecture2", "Variable qualitative: Lecteur 2",   choices=noms())
  })


  output$mytableCONCORDANCE1 <- renderTable({

    base<-BDD()

    if(input$CONCORsaisie){
      variableCONCORDANCE1 <-as.factor(strsplit(input$Concoman1," ")[[1]])
      variableCONCORDANCE2 <-as.factor(strsplit(input$Concoman2," ")[[1]])
    }else{
      variableCONCORDANCE1 <-base[,colnames(base)==input$CONCORDANCElecture1]
      variableCONCORDANCE2 <-base[,colnames(base)==input$CONCORDANCElecture2]

    }
    as.data.frame.matrix(addmargins(table(variableCONCORDANCE1,variableCONCORDANCE2)))
  },rownames=TRUE)


  output$ConcordanceManuelleINTERV <- renderPrint({

    base<-BDD()
    if(input$CONCORsaisie){
      x <-as.factor(strsplit(input$Concoman1," ")[[1]])
      y <-as.factor(strsplit(input$Concoman2," ")[[1]])
    }else{


      x <-base[,colnames(base)==input$CONCORDANCElecture1]
      y <-base[,colnames(base)==input$CONCORDANCElecture2]
    }
    # creation matrice
    Mat2<-cbind(x,y)
    Mat2<-Mat2[complete.cases(Mat2),]

    # partage des facteurs
    Mat2						<-as.data.frame(Mat2)
    LEV						<-sort(unique(c(as.character(levels(as.factor(Mat2[,1]))),as.character(levels(as.factor(Mat2[,2]))))))
    Mat2[,1]				<-as.factor(Mat2[,1])
    Mat2[,2]				<-as.factor(Mat2[,2])
    levels(Mat2[,1])	<- c(levels(Mat2[,1]),LEV[!is.element(LEV,levels(Mat2[,1]))]      )
    levels(Mat2[,2])	<- c(levels(Mat2[,2]),LEV[!is.element(LEV,levels(Mat2[,2]))]      )
    Mat2[,1]<-reorder.factor(Mat2[,1], new.order=levels(Mat2[,2]))

    if(all(Mat2[,1]==Mat2[,2])){RESULTAT<-c(1,1,1)}else{
      lkappa.boot <- function(data,x) {irr::kappa2(data[x,])$value}
      res <- boot(Mat2,lkappa.boot,10000)
      RESULTAT<-c(lkappa.boot(Mat2),boot.ci(res,type="bca")$ bca[,4:5])
    }
    cat("Le coefficient de concordance Kappa de Cohen est estimé à",RESULTAT[1],
        "dans l'intervalle à 95% [",RESULTAT[2],";",RESULTAT[3],"]\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(irr::kappa2(cbind(x,y))$p.value,3), "\n")
  })

  output$ConcordanceManuelleSimple <- renderPrint({

    base<-BDD()
    if(input$CONCORsaisie){
      x <-as.factor(strsplit(input$Concoman1," ")[[1]])
      y <-as.factor(strsplit(input$Concoman2," ")[[1]])
    }else{

      x <-base[,colnames(base)==input$CONCORDANCElecture1]
      y <-base[,colnames(base)==input$CONCORDANCElecture2]
    }
    cat("Estimation\nLe coefficient de concordance Kappa de Cohen est estimé à",round(irr::kappa2(cbind(x,y))$value,3),".
        \n\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(irr::kappa2(cbind(x,y))$p.value,3), "\n")
  })




  ########################################################################################################################
  ####    SAISIE MANUELLE ONGLET 1
  ########################################################################################################################



  output$montableauCroisemanuel <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Matrice    <- addmargins(matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes))
    colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
    rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
    Matrice },digits=0, caption = "Tableau des effectifs",
    caption.placement = getOption("xtable.caption.placement", "bottom"),
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

  output$montableauCroisemanuel2 <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Matrice    <- round(addmargins(100 * prop.table(addmargins(matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes), 1), 1), 2), 2)
    colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
    rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
    Matrice
  }, caption = "Pourcentages ligne",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

  output$montableauCroisemanuel3 <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Matrice    <- round(addmargins(100 * prop.table(addmargins(matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes), 2), 2), 1), 2)
    colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
    rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
    Matrice
  }, caption = "Pourcentages colonne",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

  output$MAINtableCHI2 <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    CH2<-stats::chisq.test(Mat,correct=FALSE)
    resTESTS<-cbind(CH2$statistic,CH2$parameter,CH2$ p.value)
    colnames(resTESTS)<-c("CHI2 Stat","CHI2 Degrés","CHI2 pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)

  output$MAINtableFISHER <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    FI2<-stats::fisher.test(Mat)
    resTESTS<-t(t( FI2$ p.value))
    colnames(resTESTS)<-c("Fisher pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)


  output$CHI2conditions <- renderText({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    CH2<-stats::chisq.test(Mat,correct=FALSE)
    FI2<-stats::fisher.test(Mat)
    ifelse(all(CH2$expected>5),"Au vu des effectifs théoriques >5, on préfèrera ici l'utilisation du test du Chi2",
           "Au vu des faibles effectifs théoriques, on préfèrera ici l'utilisation du test exact de Fisher")
  })

  output$oddratioMAIN <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){OR<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
      FI2<-stats::fisher.test(Mat)
      OR<-cbind(FI2$ estimate , FI2$ conf.int[[1]],FI2$ conf.int[[2]])
      colnames(OR)<-c("Rapport de cotes","Borne inf 2.5","Borne Sup 97.5")
      rownames(OR)<-"Résultat"}
    OR
  }, caption = "Rapport de cotes et IC",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)



  output$PerforMAIN <- renderTable({

    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){res<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

      x<-rep(c(0,1),c(Mat[1,1]+Mat[1,2],Mat[2,1]+Mat[2,2]))
      y<-rep(c(0,1,0,1),c(Mat[1,1],Mat[1,2],Mat[2,1],Mat[2,2]))
      LL<-logist(y,x)
      LLV<-data.frame(LL$Valeurs)
      Se<-LLV$VP/(LLV$VP+LLV$FN)
      Sp<-LLV$VN/(LLV$VN+LLV$FP)
      VPP<-LLV$VP/(LLV$VP+LLV$FP)
      VPN<-LLV$VN/(LLV$VN+LLV$FN)
      res<-round(data.frame(Sens=Se,Spec=Sp,VPP=VPP,VPN=VPN)*100,2)
      rownames(res)<-"Résultat"}
    res
  },rownames=TRUE)

  output$DiffDesProps <- renderTable({

    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){Res<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
      ICdiff<-IC.diff.prop(Mat[1,2], Mat[1,1]+Mat[1,2],Mat[2,2], Mat[2,1]+Mat[2,2], alpha01 = 0.5, alpha02 = 0.5, beta01 = 0.5,beta02 = 0.5, val = 0.95)
      Res<-100*cbind(ICdiff$Estimation,ICdiff$IC[3,1],ICdiff$IC[3,2])
      colnames(Res)<-c("Différence de proportions","Borne 2.5","Borne 97.5")
      rownames(Res)<-"Résultat"
    }
    Res
  }, caption = "Différence proportions et IC",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)



  output$KappaMAIN <- renderTable({
    library(boot)
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
    # une fonction pour transfo la table contingence en BDD
    countsToCases <- function(x, countcol = "Freq") {
      idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
      x[[countcol]] <- NULL
      x[idx, ]
    }
    # Mat 2 les donnees en BDD
    Mat2<-countsToCases(as.data.frame.table(Mat))

    if(all(Mat2[,1]==Mat2[,2])){RESULTAT<-c(1,1,1)}else{
      lkappa.boot <- function(data,x) {irr::kappa2(data[x,])$value}
      res <- boot(Mat2,lkappa.boot,1000)
      RESULTAT<-c(lkappa.boot(Mat2),boot.ci(res,type="bca")$ bca[,4:5])
    }
    res<-round(data.frame(Kappa=RESULTAT[1],Ic2.5=RESULTAT[2],Ic97.5=RESULTAT[3],pval=irr::kappa2(cbind(Mat2[,1],Mat2[,2]))$p.value),3)
    rownames(res)<-"Résultat"
    res
  },rownames=TRUE)

  output$KappaPlotMain <- renderPlot({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
    # une fonction pour transfo la table contingence en BDD
    countsToCases <- function(x, countcol = "Freq") {
      idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
      x[[countcol]] <- NULL
      x[idx, ]
    }
    # Mat 2 les donnees en BDD
    Mat2<-countsToCases(as.data.frame.table(Mat))

    lkappa.boot <- function(data,x) {irr::kappa2(data[x,])$value}
    res <- boot(Mat2,lkappa.boot,1000)

    # Function to plot color bar
    color.bar <- function(lut, min, max=-min, nticks=11,ticks=seq(min, max, len=nticks), title='') {
      scale = (length(lut)-1)/(max-min)


      plot(c(0,0), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', ylim=c(-0.1,1.1),main=title)
      axis(1, ticks, las=1)
      for (i in 1:(length(lut)-1)) {
        x = (i-1)/scale + min
        rect(x,0,x+1/scale,10 ,col=lut[i], border=NA)
      }
      segments( res$t0,-0.1, res$t0,1.5,lwd=8)
    }

    color.bar(colorRampPalette(c("red", "yellow", "yellow", "blue"))(100), -1)

  },height=150)


  ##### Saisi manuelle seule :
  output$mytableCONCORDANCE2 <- renderTable({

    if( is.null(input$Concoman1)) return()
    if(input$Concoman1 =="" |! length(strsplit(input$Concoman1," ")[[1]])== length(strsplit(input$Concoman2," ")[[1]])) return()

    variableCONCORDANCE1 <-as.factor(strsplit(input$Concoman1," ")[[1]])
    variableCONCORDANCE2 <-as.factor(strsplit(input$Concoman2," ")[[1]])

    as.data.frame.matrix(addmargins(table(variableCONCORDANCE1,variableCONCORDANCE2)))
  },rownames=TRUE)


  output$ConcordanceManuelleINTERV2 <- renderPrint({
    if(input$Concoman1 =="") return()
    if (! length(strsplit(input$Concoman1," ")[[1]])== length(strsplit(input$Concoman2," ")[[1]])) return("Les variables doivent avoir la même longueur")

    x <-as.factor(strsplit(input$Concoman1," ")[[1]])
    y <-as.factor(strsplit(input$Concoman2," ")[[1]])

    # creation matrice
    Mat2<-cbind(x,y)
    Mat2<-Mat2[complete.cases(Mat2),]

    # partage des facteurs
    Mat2						<-as.data.frame(Mat2)
    LEV						<-sort(unique(c(as.character(levels(as.factor(Mat2[,1]))),as.character(levels(as.factor(Mat2[,2]))))))
    Mat2[,1]				<-as.factor(Mat2[,1])
    Mat2[,2]				<-as.factor(Mat2[,2])
    levels(Mat2[,1])	<- c(levels(Mat2[,1]),LEV[!is.element(LEV,levels(Mat2[,1]))]      )
    levels(Mat2[,2])	<- c(levels(Mat2[,2]),LEV[!is.element(LEV,levels(Mat2[,2]))]      )
    Mat2[,1]<-reorder.factor(Mat2[,1], new.order=levels(Mat2[,2]))

    if(all(Mat2[,1]==Mat2[,2])){RESULTAT<-c(1,1,1)}else{
      lkappa.boot <- function(data,x) {irr::kappa2(data[x,])$value}
      res <- boot(Mat2,lkappa.boot,10000)
      RESULTAT<-c(lkappa.boot(Mat2),boot.ci(res,type="bca")$ bca[,4:5])
    }
    cat("Le coefficient de concordance Kappa de Cohen est estimé à",RESULTAT[1],
        "dans l'intervalle à 95% [",RESULTAT[2],";",RESULTAT[3],"]\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(kappa2(cbind(x,y))$p.value,3), "\n")
  })

  output$ConcordanceManuelleSimple2 <- renderPrint({
    if(input$Concoman1 =="") return()
    if (! length(strsplit(input$Concoman1," ")[[1]])== length(strsplit(input$Concoman2," ")[[1]])) return("Les variables doivent avoir la même longueur")


    x <-as.factor(strsplit(input$Concoman1," ")[[1]])
    y <-as.factor(strsplit(input$Concoman2," ")[[1]])
    cat("Estimation\nLe coefficient de concordance Kappa de Cohen est estimé à",round(irr::kappa2(cbind(x,y))$value,3),".
      \n\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(irr::kappa2(cbind(x,y))$p.value,3), "\n")
  })




  ########################################################################################################################
  ####    SAISIE MANUELLE ONGLET 1
  ########################################################################################################################



  output$montableauCroisemanuel <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Matrice    <- addmargins(matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes))
    colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
    rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
    Matrice },digits=0, caption = "Tableau des effectifs",
    caption.placement = getOption("xtable.caption.placement", "bottom"),
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

  output$montableauCroisemanuel2 <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Matrice    <- round(addmargins(100 * prop.table(addmargins(matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes), 1), 1), 2), 2)
    colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
    rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
    Matrice
  }, caption = "Pourcentages ligne",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

  output$montableauCroisemanuel3 <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Matrice    <- round(addmargins(100 * prop.table(addmargins(matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes), 2), 2), 1), 2)
    colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
    rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
    Matrice
  }, caption = "Pourcentages colonne",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

  output$MAINtableCHI2 <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    CH2<-stats::chisq.test(Mat,correct=FALSE)
    resTESTS<-cbind(CH2$statistic,CH2$parameter,CH2$ p.value)
    colnames(resTESTS)<-c("CHI2 Stat","CHI2 Degrés","CHI2 pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)

  output$MAINtableFISHER <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    FI2<-stats::fisher.test(Mat)
    resTESTS<-t(t( FI2$ p.value))
    colnames(resTESTS)<-c("Fisher pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)


  output$CHI2conditions <- renderText({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    CH2<-stats::chisq.test(Mat,correct=FALSE)
    FI2<-stats::fisher.test(Mat)
    ifelse(all(CH2$expected>5),"Au vu des effectifs théoriques >5, on préfèrera ici l'utilisation du test du Chi2",
           "Au vu des faibles effectifs théoriques, on préfèrera ici l'utilisation du test exact de Fisher")
  })

  output$oddratioMAIN <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){OR<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
      FI2<-stats::fisher.test(Mat)
      OR<-cbind(FI2$ estimate , FI2$ conf.int[[1]],FI2$ conf.int[[2]])
      colnames(OR)<-c("Rapport de cotes","Borne inf 2.5","Borne Sup 97.5")
      rownames(OR)<-"Résultat"}
    OR
  }, caption = "Rapport de cotes et IC",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)



  output$PerforMAIN <- renderTable({

    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){res<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

      x<-rep(c(0,1),c(Mat[1,1]+Mat[1,2],Mat[2,1]+Mat[2,2]))
      y<-rep(c(0,1,0,1),c(Mat[1,1],Mat[1,2],Mat[2,1],Mat[2,2]))
      LL<-logist(y,x)
      LLV<-data.frame(LL$Valeurs)
      Se<-LLV$VP/(LLV$VP+LLV$FN)
      Sp<-LLV$VN/(LLV$VN+LLV$FP)
      VPP<-LLV$VP/(LLV$VP+LLV$FP)
      VPN<-LLV$VN/(LLV$VN+LLV$FN)
      res<-round(data.frame(Sens=Se,Spec=Sp,VPP=VPP,VPN=VPN)*100,2)
      rownames(res)<-"Résultat"}
    res
  },rownames=TRUE)

  output$DiffDesProps <- renderTable({

    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){Res<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
      ICdiff<-IC.diff.prop(Mat[1,2], Mat[1,1]+Mat[1,2],Mat[2,2], Mat[2,1]+Mat[2,2], alpha01 = 0.5, alpha02 = 0.5, beta01 = 0.5,beta02 = 0.5, val = 0.95)
      Res<-100*cbind(ICdiff$Estimation,ICdiff$IC[3,1],ICdiff$IC[3,2])
      colnames(Res)<-c("Différence de proportions","Borne 2.5","Borne 97.5")
      rownames(Res)<-"Résultat"
    }
    Res
  }, caption = "Différence proportions et IC",
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)



  output$KappaMAIN <- renderTable({
    library(boot)
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
    # une fonction pour transfo la table contingence en BDD
    countsToCases <- function(x, countcol = "Freq") {
      idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
      x[[countcol]] <- NULL
      x[idx, ]
    }
    # Mat 2 les donnees en BDD
    Mat2<-countsToCases(as.data.frame.table(Mat))

    if(all(Mat2[,1]==Mat2[,2])){RESULTAT<-c(1,1,1)}else{
      lkappa.boot <- function(data,x) {irr::kappa2(data[x,])$value}
      res <- boot(Mat2,lkappa.boot,1000)
      RESULTAT<-c(lkappa.boot(Mat2),boot.ci(res,type="bca")$ bca[,4:5])
    }
    res<-round(data.frame(Kappa=RESULTAT[1],Ic2.5=RESULTAT[2],Ic97.5=RESULTAT[3],pval=irr::kappa2(cbind(Mat2[,1],Mat2[,2]))$p.value),3)
    rownames(res)<-"Résultat"
    res
  },rownames=TRUE)

  output$KappaPlotMain <- renderPlot({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
    # une fonction pour transfo la table contingence en BDD
    countsToCases <- function(x, countcol = "Freq") {
      idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
      x[[countcol]] <- NULL
      x[idx, ]
    }
    # Mat 2 les donnees en BDD
    Mat2<-countsToCases(as.data.frame.table(Mat))

    lkappa.boot <- function(data,x) {irr::kappa2(data[x,])$value}
    res <- boot(Mat2,lkappa.boot,1000)

    # Function to plot color bar
    color.bar <- function(lut, min, max=-min, nticks=11,ticks=seq(min, max, len=nticks), title='') {
      scale = (length(lut)-1)/(max-min)


      plot(c(0,0), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', ylim=c(-0.1,1.1),main=title)
      axis(1, ticks, las=1)
      for (i in 1:(length(lut)-1)) {
        x = (i-1)/scale + min
        rect(x,0,x+1/scale,10 ,col=lut[i], border=NA)
      }
      segments( res$t0,-0.1, res$t0,1.5,lwd=8)
    }

    color.bar(colorRampPalette(c("red", "yellow", "yellow", "blue"))(100), -1)

  },height=150)


  output$LandisEtKoch <- renderTable({
    data.frame(Kappa=c("0-0.2","0.21-0.40","0.41-0.60","0.61-0.80","0.81-1"),
               interpretation=c("très faible","faible","modéré","fort","presque parfait"))

  },rownames=TRUE)
  output$LandisEtKoch2 <- renderTable({
    data.frame(Kappa=c("0-0.2","0.21-0.40","0.41-0.60","0.61-0.80","0.81-1"),
               interpretation=c("très faible","faible","modéré","fort","presque parfait"))

  },rownames=TRUE)

  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  ####    PAGE GMRC
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################
  ########################################################################################################################



  # output$MOTDEPASSE <- renderText({
  # as.character(input$password)
  # })

  # output$PasSiFacile<-renderText({
  # motdepasse<- paste(format(Sys.time(), "%d"),"gmrc",sep="")
  # as.character(as.numeric(  as.character(input$password)==motdepasse))
  # })

  # ###############################################################################################################
  # ######        PAGE 1  NSN 2 props    #########################################################################
  # ###############################################################################################################



  # ########################################################################################################################
  # output$sortieNSN2props <- renderPrint({
  # pA=input$p1/100
  # pB=input$p2/100
  # kappa=input$k
  # alpha=0.05
  # beta=(100-as.numeric(input$power))/100
  # nB=ceiling((pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/2)+qnorm(1-beta))/(pA-pB))^2)
  # nA=ceiling(kappa*nB)
  # if(nA+nB<7260000000){
  # cat("Le nombre de sujets nécéssaires pour comparer", 100*pA, "% et", 100*pB,"% est estimé à",nA,"et",nB,"i.e le nombre total est d'au moins",nA+nB,".")
  # }else{cat("Le nombre de sujets est supérieur au nombre d'habitants sur Terre. Envisagez plutôt une étude multi-planétique.")
  # }
  # })


  # output$plot1NSN2props <- renderPlot({
  # par(mar = c(5.1, 4.1, 0, 1))

  # pA=input$p1/100
  # pB=input$p2/100
  # kappa=input$k
  # alpha=0.05
  # beta=(100-as.numeric(input$power))/100
  # nB=ceiling((pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/2)+qnorm(1-beta))/(pA-pB))^2)
  # nA=ceiling(kappa*nB)

  # barplot(c(nB/(nA+nB),nA/(nA+nB)),yaxt="n",col="lightcyan",ylim=1.2*c(0,max(c(nA/(nA+nB),nB/(nA+nB)))))
  # text(0.70, 0.5* nB/(nA+nB)    ,nB,cex=3)
  # text(1.90, 0.5* nA/(nA+nB)    ,nA,cex=3)
  # text( (0.70+1.9)/2, y = 1.1*max(c(nA/(nA+nB),nB/(nA+nB))),paste("Total=",nB+nA),cex=3)
  # })


  # ###############################################################################################################
  # ######        PAGE 1  NSN 2 props    #########################################################################
  # ###############################################################################################################



  # output$sortieNSN1prop <- renderPrint({
  # N=input$NunePROP
  # pA=input$p1/100
  # alpha=0.05
  # pAbasse= pA-   qnorm(1-alpha/2)*sqrt(pA*(1-pA)/N)
  # pAhaute= pA+   qnorm(1-alpha/2)*sqrt(pA*(1-pA)/N)
  # list(Proportion=pA,
  # DemiLargeur=qnorm(1-alpha/2)*sqrt(pA*(1-pA)/N),
  # Largeur=pAhaute-pAbasse)
  # })


  # output$plot1NSN1prop <- renderPlot({
  # par(mar = c(5.1, 4.1, 0, 1))

  # pA=input$p1uneprop/100
  # N=input$NunePROP
  # alpha=0.05
  # pAbasse= round(pA-   qnorm(1-alpha/2)*sqrt(pA*(1-pA)/N)    ,2)
  # pAhaute= round(pA+   qnorm(1-alpha/2)*sqrt(pA*(1-pA)/N)  ,2)


  # par(mar = c(2,1,1,1))
  # plot(c(0,0),xlim=c(0,1),ylim=c(-0.3,0.1),col="white",yaxt="n",bty="n")
  # text(pA,0,pA,cex=1.5,col="blue")
  # abline(v=pA,lty=2,col="lightgray")
  # text(pAbasse,-0.1,pAbasse)
  # text(pAhaute,-0.1,pAhaute)
  # segments(pAbasse,-0.2,pAhaute,-0.2,lwd=2)
  # segments(pAbasse,-0.21,pAbasse,-0.19,lwd=2)
  # segments(pAhaute,-0.21,pAhaute,-0.19,lwd=2)

  # })


  # ###############################################################################################################
  # ######        PAGE 2  NSN 2 moyennes  #########################################################################
  # ###############################################################################################################


  # output$sortieNSN2moys <- renderPrint({
  # mA=input$m1
  # mB=input$m2
  # sdey=input$ecarttype
  # kappa=input$kmoy
  # alpha=0.05
  # beta=(100-as.numeric(input$powerMOY))/100

  # nB=ceiling((1+1/kappa)*(sdey*(qnorm(1-alpha/2)+qnorm(1-beta))/(mA-mB))^2)
  # nA=ceiling(kappa*nB)
  # if(nA+nB<7260000000){
  # cat("Le nombre de sujets nécéssaires pour comparer", mA, "et", mB,"avec un écart-type de",sdey," est estimé à",nA,"et",nB,"i.e le nombre total est d'au moins",nA+nB,".")
  # }else{cat("Le nombre de sujets est supérieur au nombre d'habitants sur Terre. Envisagez plutôt une étude multi-planétique.")}
  # })


  # output$plot1NSN2moys <- renderPlot({
  # par(mar = c(5.1, 4.1, 0, 1))

  # mA=input$m1
  # mB=input$m2
  # sdey=input$ecarttype
  # kappa=input$kmoy
  # alpha=0.05
  # beta=(100-as.numeric(input$powerMOY))/100

  # nB=ceiling((1+1/kappa)*(sdey*(qnorm(1-alpha/2)+qnorm(1-beta))/(mA-mB))^2)
  # nA=ceiling(kappa*nB)

  # barplot(c(nB/(nA+nB),nA/(nA+nB)),yaxt="n",col="blanchedalmond",ylim=1.2*c(0,max(c(nA/(nA+nB),nB/(nA+nB)))))
  # text(0.70, 0.5* nB/(nA+nB)    ,nB,cex=3)
  # text(1.90, 0.5* nA/(nA+nB)    ,nA,cex=3)
  # text( (0.70+1.9)/2, y = 1.1*max(c(nA/(nA+nB),nB/(nA+nB))),paste("Total=",nB+nA),cex=3)
  # })




  # ###############################################################################################################
  # ######        PAGE 3  Ellicitation    #########################################################################
  # ###############################################################################################################


  # #-------------------------------------------------------
  # # Normale
  # #-------------------------------------------------------
  # output$PlotNorm <- renderPlot({
  # m <- input$mN
  # sd <- input$sN
  # minn <- m-4*sd
  # maxn <- m+4*sd
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # XX <- hist(rnorm(1000,m,sd),freq=FALSE, breaks=10)
  # YY <- curve(dnorm(x,m,sd),minn,maxn)
  # abline(v=input$mN, col="red", lwd=1.8)
  # plot(XX, freq=FALSE, xlim=c(minn,maxn), ylab="Densité", ylim=c(0,max(XX$density,YY$y)), main="", xlab="x", cex.lab=1.2, axes=FALSE)
  # axis(1)
  # axis(2, las=1)
  # lines(YY, lwd=2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pnorm(x,m,sd),minn,maxn, ylab="Probabilité cumulée",lwd=1.4, xlab="p", cex.lab=1.2, axes=FALSE)
  # axis(1)
  # axis(2, las=1)
  # points(c(minn, qnorm(0.25,m,sd)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(minn, qnorm(0.50,m,sd)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(minn, qnorm(0.75,m,sd)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qnorm(0.25,m,sd), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qnorm(0.50,m,sd), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qnorm(0.75,m,sd), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # close.screen(all.screens = TRUE)
  # })

  # ValuesNorm <- reactive({
  # data.frame(
  # p2.5 = qnorm(0.025,input$mN, input$sN),
  # Q1 = qnorm(0.25,input$mN, input$sN),
  # Médiane = qnorm(0.5,input$mN, input$sN),
  # Q3 = qnorm(0.75,input$mN, input$sN),
  # p97.5 = qnorm(0.975,input$mN, input$sN))
  # })

  # output$Norm <- renderTable({ValuesNorm()}, 'include.rownames' = FALSE, 'include.colnames' = TRUE, digits=4)

  # valuesQuantNorm <- reactive({
  # sd <- (input$k1N - input$k2N)/(qnorm(input$q1N/100)-qnorm(1-input$q2N/100))
  # XX <- c((input$k1N - sd*qnorm(input$q1N/100)), sd)
  # data.frame(m=XX[1], sd=XX[2])
  # })

  # output$QuantNorm <- renderTable({valuesQuantNorm()}, digits=c(0,4,4),'include.rownames' = FALSE)

  # output$PlotQuantNorm <- renderPlot({
  # m <- valuesQuantNorm()$m
  # sd <- valuesQuantNorm()$sd
  # minn <- m-4*sd
  # maxn <- m+4*sd
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # XX <- curve(dnorm(x, m, sd), minn, maxn)
  # plot(XX, type="l", axes=FALSE, ylab="Densité", lwd=1.4, xlab="x", cex.lab=1.2)
  # axis(1)
  # axis(2, las=1)
  # x <- seq(from=minn, to=input$k1N, length=1000)
  # y <- dnorm(x, m, sd)
  # x <- c(x, input$k1N, minn);
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # x <- seq(from=maxn, to=input$k2N, length=1000)
  # y <- dnorm(x, m, sd)
  # x <- c(x, input$k2N, maxn)
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # curve(dnorm(x,m,sd), minn, maxn, add=TRUE)
  # points(rep(input$k1N, 2), c(0, dnorm(input$k1N, m, sd)), type='l', col="red", lwd=1.2)
  # points(rep(input$k2N, 2), c(0, dnorm(input$k2N, m, sd)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pnorm(x, m, sd), minn, maxn, xlab="p", cex.lab=1.2, ylab="Probabilité cumulée", lwd=1.4, axes=F)
  # axis(1)
  # axis(2, las=1)
  # points(c(minn, qnorm(0.25, m, sd)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(minn, qnorm(0.50, m, sd)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(minn, qnorm(0.75, m, sd)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qnorm(0.25, m, sd), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qnorm(0.50, m, sd), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qnorm(0.75, m, sd), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(input$k1N, 2), c(0, pnorm(input$k1N, m, sd)), type='l', col="red", lwd=1.2)
  # points(rep(input$k2N, 2), c(0, pnorm(input$k2N, m, sd)), type='l', col="red", lwd=1.2)
  # points(c(minn,input$k1N), rep(pnorm(input$k1N, m, sd),2), type='l', col="red", lwd=1.2)
  # points(c(minn,input$k2N), rep(pnorm(input$k2N, m, sd),2), type='l', col="red", lwd=1.2)
  # close.screen(all.screens = TRUE)
  # })

  # valueMinMaxNorm <- reactive({
  # data.frame(
  # Moyenne = (input$minN+input$maxN+2*input$medN)/4,
  # Ecart.type = (input$maxN-input$minN)/4)
  # })

  # output$MinMaxNorm <- renderTable({valueMinMaxNorm()}, 'include.rownames' = FALSE, 'include.colnames' = TRUE, digits=4)

  # output$PlotMinMaxNorm <- renderPlot({
  # m <- valueMinMaxNorm()$Moyenne
  # sd <- valueMinMaxNorm()$Ecart.type
  # minn <- m-4*sd
  # maxn <- m+4*sd
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # XX <- curve(dnorm(x, m, sd), minn, maxn)
  # plot(XX, type="l", axes=FALSE, ylab="Densité", lwd=1.4, xlab="x", cex.lab=1.2)
  # axis(1)
  # axis(2, las=1)
  # points(rep(input$minN, 2), c(0, dnorm(input$minN, m, sd)), type='l', col="red", lwd=1.2)
  # points(rep(input$maxN, 2), c(0, dnorm(input$maxN, m, sd)), type='l', col="red", lwd=1.2)
  # points(rep(input$medN, 2), c(0, dnorm(input$medN, m, sd)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pnorm(x, m, sd), minn, maxn, xlab="p", cex.lab=1.2, ylab="Probabilité cumulée", lwd=1.4, axes=F)
  # axis(1)
  # axis(2, las=1)
  # points(c(minn, qnorm(0.25, m, sd)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(minn, qnorm(0.50, m, sd)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(minn, qnorm(0.75, m, sd)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qnorm(0.25, m, sd), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qnorm(0.50, m, sd), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qnorm(0.75, m, sd), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(input$minN, 2), c(0, pnorm(input$minN, m, sd)), type='l', col="red", lwd=1.2)
  # points(rep(input$maxN, 2), c(0, pnorm(input$maxN, m, sd)), type='l', col="red", lwd=1.2)
  # points(rep(input$medN, 2), c(0, pnorm(input$medN, m, sd)), type='l', col="red", lwd=1.2)
  # points(c(minn,input$minN), rep(pnorm(input$minN, m, sd),2), type='l', col="red", lwd=1.2)
  # points(c(minn,input$maxN), rep(pnorm(input$maxN, m, sd),2), type='l', col="red", lwd=1.2)
  # points(c(minn,input$medN), rep(pnorm(input$medN, m, sd),2), type='l', col="red", lwd=1.2)
  # close.screen(all.screens = TRUE)
  # })


  # #-------------------------------------------------------
  # # Beta
  # #-------------------------------------------------------
  # sliderValuesBetaMS <- reactive({
  # AB <- parametres.beta(input$mB,input$sB)
  # data.frame(
  # Forme1 = AB$alpha,
  # Forme2 = AB$beta)
  # })

  # output$valuesBetaMS <- renderTable({sliderValuesBetaMS()}, digits=c(0,4,4),'include.rownames' = FALSE)

  # output$PlotBetaMS <- renderPlot({
  # AB <- parametres.beta(input$mB,input$sB)
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # XX <- curve(dbeta(x,AB$alpha,AB$beta),0.001, 0.999)
  # plot(XX, type="l", axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2)
  # axis(1)
  # axis(2, las=1)
  # points(rep(input$mB, 2), c(0, dbeta(input$mB, AB$alpha, AB$beta)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pbeta(x, AB$alpha, AB$beta), from = 0.001, to = 0.999, xlim=c(0,1), xlab="p", cex.lab=1.2, ylab="Probabilité cumulée", lwd=1.4, axes=FALSE)
  # axis(1)
  # axis(2, las=1)
  # points(c(0, qbeta(0.25, AB$alpha, AB$beta)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(0, qbeta(0.50, AB$alpha, AB$beta)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(0, qbeta(0.75, AB$alpha, AB$beta)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qbeta(0.25, AB$alpha, AB$beta), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qbeta(0.50, AB$alpha, AB$beta), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qbeta(0.75, AB$alpha, AB$beta), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # close.screen(all.screens = TRUE)
  # })

  # valuesQuantBeta <- reactive({
  # XX <- parms.quantiles(k=c(input$k1B/100,input$k2B/100), q=c(input$q1B/100,(1-input$q2B/100)), distrib="beta")
  # data.frame(alpha=XX$param[1], beta=XX$param[2])
  # })

  # output$QuantBeta <- renderTable({valuesQuantBeta()}, digits=c(0,4,4),'include.rownames' = FALSE)

  # output$PlotQuantBeta <- renderPlot({
  # aa <- valuesQuantBeta()$alpha
  # bb <- valuesQuantBeta()$beta
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # XX <- curve(dbeta(x,aa,bb),0,1)
  # plot(XX, type="l", axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2)
  # axis(1, at=seq(0, 1, by=0.2))
  # axis(2, at=seq(0, max(XX$y), by=0.2), las=1)
  # x <- seq(from=0, to=input$k1B/100, length=1000)
  # y <- dbeta(x,aa, bb)
  # x <- c(x, input$k1B/100, 0);
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # x <- seq(from=1, to=input$k2B/100, length=1000)
  # y <- dbeta(x,aa, bb)
  # x <- c(x, input$k2B/100, 1)
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # curve(dbeta(x,aa,bb),0,1,add=TRUE)
  # points(rep(input$k1B/100, 2), c(0, dbeta(input$k1B/100,aa, bb)), type='l', col="red", lwd=1.2)
  # points(rep(input$k2B/100, 2), c(0, dbeta(input$k2B/100,aa, bb)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pbeta(x,aa, bb), from = 0.01, to = 0.99, xlim=c(0,1), xlab="p", cex.lab=1.2, ylab="Probabilité cumulée", lwd=1.4, axes=F)
  # axis(1, at=seq(0, 1, by=0.2))
  # axis(2, at=seq(0, 1, by=0.2), las=1)
  # points(c(0, qbeta(0.25, aa, bb)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(0, qbeta(0.50, aa, bb)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(0, qbeta(0.75, aa, bb)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qbeta(0.25, aa, bb), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qbeta(0.50, aa, bb), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qbeta(0.75, aa, bb), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(input$k1B/100, 2), c(0, pbeta(input$k1B/100,aa, bb)), type='l', col="red", lwd=1.2)
  # points(rep(input$k2B/100, 2), c(0, pbeta(input$k2B/100,aa, bb)), type='l', col="red", lwd=1.2)
  # points(c(0,input$k1B/100), rep(pbeta(input$k1B/100,aa, bb),2), type='l', col="red", lwd=1.2)
  # points(c(0,input$k2B/100), rep(pbeta(input$k2B/100,aa, bb),2), type='l', col="red", lwd=1.2)
  # close.screen(all.screens = TRUE)
  # })

  # #-------------------------------------------------------
  # #Gamma
  # #-------------------------------------------------------
  # ValuesGammaMS <- reactive({
  # AB <- parametres.gamma(input$mG,input$sG)
  # data.frame(
  # Shape = AB$Shape,
  # Rate = AB$Rate,
  # Scale = 1/AB$Rate
  # )
  # })

  # output$valuesGammaMS <- renderTable({ValuesGammaMS()}, digits=c(0,4,4,4),'include.rownames' = FALSE)

  # output$PlotGammaMS <- renderPlot({
  # AB <- parametres.gamma(input$mG,input$sG)
  # xMax <- qgamma(0.9999, shape=AB$Shape, rate=AB$Rate)
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # XX <- curve(dgamma(x, shape=AB$Shape, rate=AB$Rate), 0, xMax)
  # plot(XX, type="l", axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2)
  # axis(1)
  # axis(2, las=1)
  # points(rep(input$mG, 2), c(0, dgamma(input$mG, shape=AB$Shape, rate=AB$Rate)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pgamma(x, shape=AB$Shape, rate=AB$Rate), 0, xMax, xlab="p", cex.lab=1.2, ylab="Probabilité cumulée", lwd=1.4, axes=FALSE)
  # axis(1)
  # axis(2, las=1)
  # points(c(0, qgamma(0.25, shape=AB$Shape, rate=AB$Rate)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(0, qgamma(0.50, shape=AB$Shape, rate=AB$Rate)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(0, qgamma(0.75, shape=AB$Shape, rate=AB$Rate)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qgamma(0.25, shape=AB$Shape, rate=AB$Rate), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qgamma(0.50, shape=AB$Shape, rate=AB$Rate), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qgamma(0.75, shape=AB$Shape, rate=AB$Rate), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # close.screen(all.screens = TRUE)
  # })

  # valuesQuantGamma <- reactive({
  # XX <- parms.quantiles(k=c(input$k1G,input$k2G), q=c(input$q1G/100,(1-input$q2G/100)), distrib="gamma")
  # data.frame(Shape=XX$param[1], Rate=XX$param[2], Scale=1/XX$param[2])
  # })

  # output$QuantGamma <- renderTable({valuesQuantGamma()}, digits=c(0,4,4,4),'include.rownames' = FALSE)

  # output$PlotQuantGamma <- renderPlot({
  # aa <- valuesQuantGamma()$Shape
  # bb <- valuesQuantGamma()$Rate
  # xMax <- qgamma(0.999, shape=aa, rate=bb)
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # XX <- curve(dgamma(x,shape=aa, rate=bb), 0, xMax)
  # plot(XX, type="l", axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2)
  # axis(1)
  # axis(2, las=1)
  # x <- seq(from=0, to=input$k1G, length=1000)
  # y <- dgamma(x, shape=aa, rate=bb)
  # x <- c(x, input$k1G, 0);
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # x <- seq(from=xMax, to=input$k2G, length=1000)
  # y <- dgamma(x, shape=aa, rate=bb)
  # x <- c(x, input$k2G, xMax)
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # curve(dgamma(x,shape=aa, rate=bb), 0, xMax, add=TRUE)
  # points(rep(input$k1G, 2), c(0, dgamma(input$k1G, shape=aa, rate=bb)), type='l', col="red", lwd=1.2)
  # points(rep(input$k2G, 2), c(0, dgamma(input$k2G, shape=aa, rate=bb)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pgamma(x, shape=aa, rate=bb), 0, xMax, xlim=c(0, xMax), xlab="p", cex.lab=1.2, ylab="Probabilité cumulée", lwd=1.4, axes=F)
  # axis(1)
  # axis(2, las=1)
  # points(c(0, qgamma(0.25, shape=aa, rate=bb)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(0, qgamma(0.50, shape=aa, rate=bb)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(0, qgamma(0.75, shape=aa, rate=bb)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qgamma(0.25, shape=aa, rate=bb), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qgamma(0.50, shape=aa, rate=bb), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qgamma(0.75, shape=aa, rate=bb), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(input$k1G, 2), c(0, pgamma(input$k1G, shape=aa, rate=bb)), type='l', col="red", lwd=1.2)
  # points(rep(input$k2G, 2), c(0, pgamma(input$k2G, shape=aa, rate=bb)), type='l', col="red", lwd=1.2)
  # points(c(0,input$k1G), rep(pgamma(input$k1G, shape=aa, rate=bb),2), type='l', col="red", lwd=1.2)
  # points(c(0,input$k2G), rep(pgamma(input$k2G, shape=aa, rate=bb),2), type='l', col="red", lwd=1.2)
  # close.screen(all.screens = TRUE)
  # })

  # #-------------------------------------------------------
  # #TAU et SIGMA2
  # #-------------------------------------------------------
  # ValuesTauSd <- reactive({
  # AB <- parametres.gamma(input$mTau,input$sTau)
  # S2 <- 1/rgamma(10000, shape=AB$Shape, rate=AB$Rate)
  # data.frame(
  # Shape.Tau = AB$Shape,
  # Rate.Tau = AB$Rate,
  # Scale.Tau = 1/AB$Rate,
  # Moyenne.S2 = mean(S2),
  # Sd.S2 = sd(S2))
  # })

  # output$TauSd <- renderTable({ValuesTauSd()}, digits=c(0,4,4,4,4,4), 'include.rownames' = FALSE)

  # output$PlotTauSd<- renderPlot({
  # AB <- ValuesTauSd()
  # xMax <- qgamma(0.9999, shape=AB$Shape.Tau, rate=AB$Rate.Tau)
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # curve(dgamma(x, shape=AB$Shape.Tau, rate=AB$Rate.Tau), 0, xMax, axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2, main="Précision")
  # axis(1)
  # axis(2, las=1)
  # points(rep(input$mTau, 2), c(0, dgamma(input$mTau, shape=AB$Shape.Tau, rate=AB$Rate.Tau)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # YY <- 1/rgamma(10000, shape=AB$Shape.Tau, rate=AB$Rate.Tau)
  # ZZ <- curve(dinvgamma(x, shape=AB$Shape.Tau, rate=AB$Rate.Tau), min(YY), max(YY), axes=FALSE, ylab="Densité", lwd=1.4, cex.lab=1.2, xlab="", main="Variance")
  # hist(YY, axes=FALSE, xlim=c(min(YY), max(YY)), ylim=c(0,max(ZZ$y)), freq=FALSE, add=TRUE)
  # axis(1)
  # axis(2, las=1)
  # close.screen(all.screens = TRUE)
  # })

  # ValuesS2Tau <- reactive({
  # AB <- NR.MS(input$mS2, input$sS2)
  # data.frame(
  # Shape.S2 = AB$shape,
  # Rate.S2 = AB$rate,
  # Scale.S2 = 1/AB$rate,
  # Shape.Tau = AB$shape,
  # Rate.Tau = 1/AB$rate,
  # Scale.Tau = AB$rate,
  # Moyenne.Tau = AB$shape/AB$rate,
  # Var.Tau = AB$shape/AB$rate/AB$rate)
  # })

  # ValuesSummaryS2Tau <- reactive({
  # s.S2 <- rinvgamma(10000, shape=ValuesS2Tau()$Shape.S2, rate=ValuesS2Tau()$Rate.S2)
  # s.Tau <- rgamma(10000, shape=ValuesS2Tau()$Shape.Tau, rate=1/ValuesS2Tau()$Rate.Tau)
  # XX<-data.frame(
  # p2.5=c(quantile(s.S2,0.025), quantile(s.Tau,0.025)),
  # p25=c(quantile(s.S2,0.25), quantile(s.Tau,0.25)),
  # p50=c(quantile(s.S2,0.5), quantile(s.Tau,0.5)),
  # Moy=c(mean(s.S2), mean(s.Tau)),
  # p75=c(quantile(s.S2,0.725), quantile(s.Tau,0.75)),
  # p97.5=c(quantile(s.S2,0.925), quantile(s.Tau,0.925)),
  # Var=c(var(s.S2), var(s.Tau)),
  # Sd=c(sd(s.S2), sd(s.Tau)))
  # row.names(XX) <- c("Variance", "Précision")
  # return(XX)
  # })

  # output$S2Tau <- renderTable({ValuesS2Tau()}, digits=c(0,4,4,4,4,4,4,4,6), 'include.rownames' = FALSE)
  # output$summaryS2Tau  <- renderTable({ValuesSummaryS2Tau()}, digits=c(0, rep(4,8)))

  # output$PlotS2Tau<- renderPlot({
  # AB <- ValuesS2Tau()
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qinvgamma(0.005, shape=AB$Shape.S2, rate=AB$Rate.S2)
  # xMin <- qinvgamma(0.995, shape=AB$Shape.S2, rate=AB$Rate.S2)
  # curve(dinvgamma(x, shape=AB$Shape.S2, rate=AB$Rate.S2), xMin, xMax, axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2, main="Variance")
  # axis(1)
  # axis(2, las=1)
  # points(rep(input$mS2, 2), c(0, dinvgamma(input$mS2, shape=AB$Shape.S2, rate=AB$Rate.S2)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qgamma(0.005, shape=AB$Shape.S2, rate=AB$Rate.S2)
  # xMin <- qgamma(0.995, shape=AB$Shape.S2, rate=AB$Rate.S2)
  # curve(dgamma(x, shape=AB$Shape.S2, rate=AB$Rate.S2), xMin, xMax, axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2, main="Précision")
  # MM <- AB$Shape.S2/AB$Rate.S2
  # points(rep(MM, 2), c(0, dgamma(MM, shape=AB$Shape.S2, rate=AB$Rate.S2)), type='l', col="red", lwd=1.2)
  # axis(1)
  # axis(2, las=1)
  # close.screen(all.screens = TRUE)
  # })


  # #-------------------------------------------------------
  # # Exponentielle
  # #-------------------------------------------------------
  # ValuesExp <- reactive({
  # data.frame(
  # lambda = 1/input$mE,
  # Ecart.type = input$mE,
  # p2.5 = qexp(0.025, 1/input$mE),
  # Q1 = qexp(0.25, 1/input$mE),
  # Médiane = qexp(0.5, 1/input$mE),
  # Q3 = qexp(0.75, 1/input$mE),
  # p97.5 = qexp(0.975, 1/input$mE))
  # })

  # output$Exp <- renderTable({ValuesExp()}, 'include.rownames' = FALSE, 'include.colnames' = TRUE, digits=4)

  # output$PlotExp <- renderPlot({
  # minn <- 0
  # maxn <- 5*input$mE
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # curve(dexp(x, 1/input$mE), minn, maxn, ylab="Densité", xlab="x", cex.lab=1.2, axes=FALSE)
  # points(rep(input$mE,2),c(0,dexp(input$mE,1/input$mE)), col="red", lwd=1.2, type="l")
  # axis(1)
  # axis(2, las=1)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # curve(pexp(x, 1/input$mE),minn,maxn, ylab="Probabilité cumulée",lwd=1.4, xlab="p", cex.lab=1.2, axes=FALSE)
  # axis(1)
  # axis(2, ylim=c(0,1), las=1)
  # points(c(minn, qexp(0.25,1/input$mE)), rep(0.25 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(c(minn, qexp(0.50,1/input$mE)), rep(0.50 ,2), type='l', lwd=1.2, lty=2)
  # points(c(minn, qexp(0.75,1/input$mE)), rep(0.75 ,2), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qexp(0.25,1/input$mE), 2), c(0, 0.25), type='l', col="blue", lwd=1.2, lty=2)
  # points(rep(qexp(0.50,1/input$mE), 2), c(0, 0.50), type='l', lwd=1.2, lty=2)
  # points(rep(qexp(0.75,1/input$mE), 2), c(0, 0.75), type='l', col="blue", lwd=1.2, lty=2)
  # close.screen(all.screens = TRUE)
  # })

  # #-------------------------------------------------------
  # # Odds-ratio
  # #-------------------------------------------------------
  # ValuesRegLogBeta <- reactive({
  # YY <- qnorm(c(0.025,0.25,0.5,0.75,0.975), input$mBeta, input$sBeta)
  # ZZ <- qlnorm(c(0.025,0.25,0.5,0.75,0.975), input$mBeta, input$sBeta)
  # XX<-data.frame(
  # p2.5=c(YY[1], ZZ[1], NA),
  # p25=c(YY[2],  ZZ[2], NA),
  # p50=c(YY[3],  ZZ[3], NA),
  # Moy=c(input$mBeta, exp(input$mBeta), exp(input$mBeta+(input$sBeta)^2/2)),
  # p75=c(YY[4],  ZZ[4], NA),
  # p97.5=c(YY[5],  ZZ[5], NA),
  # Var=c((input$sBeta)^2, NA, (exp(2*input$mBeta + 2*input$sBeta^2)-exp(2*input$mBeta + input$sBeta^2))),
  # Sd=c(input$sBeta, NA, sqrt(exp(2*input$mBeta + 2*input$sBeta^2)-exp(2*input$mBeta + input$sBeta^2))))
  # row.names(XX) <- c("Beta", "Odds-ratio", "OR Observé")
  # return(XX)
  # })

  # output$RegLogBeta <- renderTable({ValuesRegLogBeta()}, digits=4)

  # output$PlotRegLogBeta<- renderPlot({
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qnorm(0.995, input$mBeta, input$sBeta)
  # xMin <- qnorm(0.005, input$mBeta, input$sBeta)
  # XX <- curve(dnorm(x, input$mBeta, input$sBeta), xMin, xMax, axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2, main="Beta")
  # axis(1)
  # axis(2, las=1)
  # points(rep(input$mBeta, 2), c(0, dnorm(input$mBeta, input$mBeta, input$sBeta)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qlnorm(0.95, input$mBeta, input$sBeta)
  # xMin <- qlnorm(0.05, input$mBeta, input$sBeta)
  # MM <- exp(input$mBeta+(input$sBeta)^2/2)
  # curve(dlnorm(x,input$mBeta, input$sBeta), xMin , xMax, axes=FALSE, cex.lab=1.2, lwd=1.4, main="Odds-ratio", ylab="Densité")
  # points(rep(MM, 2), c(0, dlnorm(MM, input$mBeta, input$sBeta)), type='l', col="red", lwd=1.2)
  # axis(1)
  # axis(2, las=1)
  # close.screen(all.screens = TRUE)
  # })

  # ValuesLOR.MS <- reactive({
  # XX <- data.frame(
  # Moyenne = log(input$mOR) - 1/2*log(1+input$sOR^2/input$mOR^2),
  # Ecart.type = sqrt(log(1+input$sOR^2/input$mOR^2)))
  # row.names(XX) <- "Beta"
  # return(XX)
  # })

  # output$LOR.MS <- renderTable({ValuesLOR.MS()})

  # ValuesOR.MS <- reactive({
  # YY <- qlnorm(c(0.025,0.25,0.5,0.75,0.975), ValuesLOR.MS()$Moyenne, ValuesLOR.MS()$Ecart.type)
  # XX<-data.frame(
  # p2.5=YY[1],
  # p25=YY[2],
  # p50=YY[3],
  # Moy=input$mOR,
  # p75=YY[4],
  # p97.5=YY[5],
  # Var=input$sOR^2,
  # Sd=input$sOR)
  # row.names(XX) <- "Odds-ratio"
  # return(XX)
  # })

  # output$OR.MS <- renderTable({ValuesOR.MS()})

  # output$PlotLOR.MS<- renderPlot({
  # Moy <- ValuesLOR.MS()$Moyenne
  # Et <- ValuesLOR.MS()$Ecart.type
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qlnorm(0.995, Moy, Et)
  # xMin <- qlnorm(0.005, Moy, Et)
  # XX <- curve(dlnorm(x, Moy, Et), xMin, xMax, axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2, main="Odds-ratio")
  # axis(1)
  # axis(2, las=1)
  # points(rep(input$mOR, 2), c(0, dlnorm(input$mOR, Moy, Et)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qnorm(0.995, Moy, Et)
  # xMin <- qnorm(0.005, Moy, Et)
  # XX <- curve(dnorm(x, Moy, Et), xMin, xMax, axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2, main="Log-odds ou beta")
  # axis(1)
  # axis(2, las=1)
  # points(rep(Moy, 2), c(0, dnorm(Moy, Moy, Et)), type='l', col="red", lwd=1.2)
  # close.screen(all.screens = TRUE)
  # })

  # ValuesQuantLOR <- reactive({
  # Sd <- (log(input$k1OR) - log(input$k2OR))/(qnorm(input$q1OR/100)-qnorm(1-input$q2OR/100))
  # Moy <- log(input$k1OR) - Sd*qnorm(input$q1OR/100)
  # XX <- data.frame(
  # Moyenne = Moy,
  # Ecart.type = Sd)
  # row.names(XX) <- "Beta"
  # return(XX)
  # })

  # output$QuantLOR <- renderTable({ValuesQuantLOR()})

  # ValuesQuantOR <- reactive({
  # YY <- qlnorm(c(0.025,0.25,0.5,0.75,0.975), ValuesQuantLOR()$Moyenne, ValuesQuantLOR()$Ecart.type)
  # XX<-data.frame(
  # p2.5=YY[1],
  # p25=YY[2],
  # p50=YY[3],
  # Moy=Moy,
  # p75=YY[4],
  # p97.5=YY[5],
  # Var=Sd^2,
  # Sd=Sd)
  # row.names(XX) <- "Odds-ratio"
  # return(XX)
  # })

  # output$QuantOR <- renderTable({ValuesQuantOR()}, digits=4)

  # output$PlotQuantOR<- renderPlot({
  # MoyN <- ValuesQuantLOR()$Moyenne
  # SdN <- ValuesQuantLOR()$Ecart.type
  # Moy <- exp(MoyN + 1/2*SdN^2)
  # Sd <- sqrt(exp(2*(MoyN+SdN^2))-exp(2*MoyN+SdN^2))
  # split.screen(rbind(c(0.1,0.5,0.1, 0.98), c(0.52, 0.95, 0.1, 0.98)))
  # screen(1)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qlnorm(0.9999, MoyN, SdN)
  # xMin <- qlnorm(0.0001, MoyN, SdN)
  # XX <- curve(dlnorm(x, MoyN, SdN), xMin, xMax)
  # plot(XX, type="l", axes=FALSE, lwd=1.4, ylab="Densité", xlab="x", cex.lab=1.2, main="Odds-ratio")
  # axis(1)
  # axis(2, las=1)
  # x <- seq(from=xMin, to=input$k1OR, length=1000)
  # y <- dlnorm(x, MoyN, SdN)
  # x <- c(x, input$k1OR, xMin);
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # x <- seq(from=xMax, to=input$k2OR, length=1000)
  # y <- dlnorm(x, MoyN, SdN)
  # x <- c(x, input$k2OR, xMax)
  # y <- c(y, 0, 0)
  # polygon(x, y, col='lightgrey', border='lightgray')
  # curve(dlnorm(x, MoyN, SdN), xMin, xMax, add=TRUE)
  # points(rep(input$k1OR, 2), c(0, dlnorm(input$k1OR, MoyN, SdN)), type='l', col="red", lwd=1.2)
  # points(rep(input$k2OR, 2), c(0, dlnorm(input$k2OR, MoyN, SdN)), type='l', col="red", lwd=1.2)
  # screen(2)
  # par(mar=c(2, 4, 1, 1))
  # xMax <- qnorm(0.995, MoyN, SdN)
  # xMin <- qnorm(0.005, MoyN, SdN)
  # curve(dnorm(x, MoyN, SdN), xMin, xMax, xlab="p", cex.lab=1.2, ylab="Densité", lwd=1.4, axes=F, main="Log-odds ou beta")
  # axis(1)
  # axis(2, las=1)
  # points(rep(MoyN, 2), c(0, dnorm(MoyN, MoyN, SdN)), type='l', col="red", lwd=1.2)
  # close.screen(all.screens = TRUE)
  # })










})








# #-------------------------------------------------------
# # Fonctions pour les pages GMRC ellicitation
# #-------------------------------------------------------
# parametres.beta<-function(m,sd){
# alpha <- m^2*(1-m)/(sd^2)-m
# beta  <- (1-m)/m*(alpha)
# return(list(alpha=alpha,beta=beta))
# }

# parametres.gamma<-function(m,sd){
# alpha <- m^2/sd^2
# beta  <- m/sd^2
# return(list(Shape=alpha, Rate=beta))
# }

# rinvgamma <- function(n, shape, rate){return(1/rgamma(n, shape=shape, rate=rate))}
# pinvgamma <- function(q, shape, rate){return(pgamma(1/q, shape=shape, rate=rate))}
# qinvgamma <- function(p, shape, rate){return(1/qgamma(p, shape=shape, rate=rate))}
# dinvgamma <- function(x, shape, rate){return(exp(shape*log(rate)-lgamma(shape)-(shape+1)*log(x)-rate/x))}

# parametres.invgamma <- function(m, sd){
# alphaIG  <- m^2/sd^2+2
# betaIG <- (alphaIG-1)*m
# return(list(shape.IG=alphaIG, rate.IG=1/betaIG))
# }

# parms.quantiles <- function(k, q=c(0.025,0.975), distrib, precision=0.001, derivative.epsilon=1e-3){
# # Function developed by Lawrence Joseph and Patrick Belisle
# # patrick.belisle@clinepi.mcgill.ca
# if(distrib=="beta"){
# f.cum <- function(x, theta){pbeta(x, shape1=theta[1], shape2=theta[2])}
# theta.from.moments <- function(m, v){a <- m*m*(1-m)/v-m; b <- a*(1/m-1); c(a, b)}
# }
# if(distrib=="gamma"){
# f.cum <- function(x, theta){pgamma(x, shape=theta[1], rate=theta[2])}
# theta.from.moments <- function(m, v){shape <- m*m/v; rate <- m/v; c(shape, rate)}
# }
# k <- sort(k); q <- sort(q)
# Hessian <- matrix(NA, 2, 2)
# m <-  diff(k)/diff(q)*(0.5-q[1]) + k[1]
# v <- (diff(k)/diff(qnorm(q)))^2
# theta <- theta.from.moments(m, v)
# change <- precision + 1
# niter <- 0
# while (max(abs(change)) > precision){
# Hessian[,1] <- (f.cum(k, theta) - f.cum(k, theta - c(derivative.epsilon, 0))) / derivative.epsilon
# Hessian[,2] <- (f.cum(k, theta) - f.cum(k, theta - c(0, derivative.epsilon))) / derivative.epsilon
# f <- f.cum(k, theta) - q
# change <- solve(Hessian) %*% f
# last.theta <- theta
# theta <- last.theta - change
# if (any(theta<0)){
# ee <- min(last.theta/change)
# theta <- last.theta - ee/2*change
# }
# niter <- niter + 1
# }
# return(list(param=c(theta[1], theta[2]))) # shape, rate
# }

# NR.MS <- function(moy, et, precision=0.005, derivative.epsilon=1e-3, S=2000){
# rinvgamma <- function(n, shape, rate){return(1/rgamma(n, shape=shape, rate=rate))}
# k <- c(moy,et)
# Hessian <- matrix(NA, 2, 2)
# #init
# alphaIG <- moy^2/et^2+2
# betaIG <- (alphaIG-1)*moy
# theta <-c(alphaIG, betaIG)
# #fonction renvoyant moy et sd
# f.cum <- function(theta){
# XX <- rinvgamma(S, shape=theta[1], rate=theta[2])
# return(c(mean(XX), sd(XX)))
# }
# change <- precision + 1
# niter <- 0
# while (max(abs(change)) > precision){
# F.cum <- f.cum(theta)
# Hessian[,1] <- (F.cum - f.cum(theta - c(derivative.epsilon, 0))) / derivative.epsilon
# Hessian[,2] <- (F.cum - f.cum(theta - c(0, derivative.epsilon))) / derivative.epsilon
# f <- F.cum - k
# change <- solve(Hessian) %*% f
# last.theta <- theta
# theta <- last.theta - change
# if (any(theta<0)){
# ee <- min(last.theta/change)
# theta <- last.theta - ee/2*change
# }
# niter <- niter + 1
# }
# return(list(shape=theta[1], rate=theta[2]))
# }






