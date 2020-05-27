

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
if(!require(gmrcfun)){install_github(repo = "jgodet/gmrcfun")}; library(gmrcfun)

server <- shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })





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
      do.call(tabPanel,pasDeBase)
    }else{
      do.call(tabPanel,univarie)
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
      do.call(tabPanel,pasDeBase)
    }else{
      do.call(tabPanel,croisementsInference)
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
    CH2<-chisq.test(Mat,correct=FALSE)
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
    FI2<-fisher.test(Mat)
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
    CH2<-chisq.test(Mat,correct=FALSE)
    FI2<-fisher.test(Mat)
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
      FI2<-fisher.test(Mat)
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
      do.call(tabPanel,pasDeBase)
    }else{
      do.call(tabPanel,analyseDeSurvie)
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
      do.call(tabPanel,pasDeBase)
    }else{
      do.call(tabPanel,testsDiagnostiques)
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

    y2                              <-y
    x2                              <-ifelse(x>best.cut,1,0)
    T                               <-table(x2,y2)
    rownames(T)<-c("x<cut","x>cut")
    colnames(T)<-c("0","1")
    T
  },rownames=TRUE)







  ########################################################################################################################
  ####    OUTPUT page 7 CONCORDANCE
  ########################################################################################################################


  output$concordance = renderUI({
    if(!BASEchargee()){
      do.call(tabPanel,concordanceSansBase)
    }else{
      do.call(tabPanel,concordanceAvecBase)
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

    CH2<-chisq.test(Mat,correct=FALSE)
    resTESTS<-cbind(CH2$statistic,CH2$parameter,CH2$ p.value)
    colnames(resTESTS)<-c("CHI2 Stat","CHI2 Degrés","CHI2 pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)

  output$MAINtableFISHER <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    FI2<-fisher.test(Mat)
    resTESTS<-t(t( FI2$ p.value))
    colnames(resTESTS)<-c("Fisher pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)


  output$CHI2conditions <- renderText({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    CH2<-chisq.test(Mat,correct=FALSE)
    FI2<-fisher.test(Mat)
    ifelse(all(CH2$expected>5),"Au vu des effectifs théoriques >5, on préfèrera ici l'utilisation du test du Chi2",
           "Au vu des faibles effectifs théoriques, on préfèrera ici l'utilisation du test exact de Fisher")
  })

  output$oddratioMAIN <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){OR<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
      FI2<-fisher.test(Mat)
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

    CH2<-chisq.test(Mat,correct=FALSE)
    resTESTS<-cbind(CH2$statistic,CH2$parameter,CH2$ p.value)
    colnames(resTESTS)<-c("CHI2 Stat","CHI2 Degrés","CHI2 pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)

  output$MAINtableFISHER <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    FI2<-fisher.test(Mat)
    resTESTS<-t(t( FI2$ p.value))
    colnames(resTESTS)<-c("Fisher pValue")
    rownames(resTESTS)<-"Résultat"
    resTESTS
  },rownames=TRUE)


  output$CHI2conditions <- renderText({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN

    Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)

    CH2<-chisq.test(Mat,correct=FALSE)
    FI2<-fisher.test(Mat)
    ifelse(all(CH2$expected>5),"Au vu des effectifs théoriques >5, on préfèrera ici l'utilisation du test du Chi2",
           "Au vu des faibles effectifs théoriques, on préfèrera ici l'utilisation du test exact de Fisher")
  })

  output$oddratioMAIN <- renderTable({
    Nblignes   <-input$NbLignesMAIN
    Nbcolonnes <-input$NbcolonnesMAIN
    if(Nblignes>2 | Nbcolonnes>2){OR<-NULL}else{
      Mat<-matrix(as.numeric(strsplit(input$TableauMAIN1," ")[[1]]),ncol=Nbcolonnes,nrow=Nblignes)
      FI2<-fisher.test(Mat)
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

})


univarie <-

  fluidPage(

    navbarPage(title = NULL,id="descriptif",

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
                                     plotOutput("plotNAbase3"),
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
                              radioButtons("qualiquanti", "Nature de la variable",
                                           c(Quantitative="quant", Qualitative="qual"),"qual"
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
                                column(6,     plotOutput("plot1") , plotOutput("plot2") )
                              )# fin fluid row du main panel

                            )# fin MainPanel

                          )# fin sidebarlayout
                        )# fin fluidpage
               )# fin tabPanel 2

    )# fin navbarPage

  )



analyseDeSurvie <-

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
        plotOutput("plotSURVIE"),
        p("Le détail des données utilisées pour la construction de cette ou ces courbes est présenté ci-dessous. Dans le cas
                                                        d'une comparaison entre plusieurs groupes, le détail est présenté par groupes, un test d'égalité de l'ensemble des courbes est
                                                        présenté (Test du Log-Rank) et les résultats sont affichés au bas de cette page."),
        h3("Valeurs numériques de survie: analyses détaillées"),
        verbatimTextOutput ("sortieSURVIE2"))# fin MainPanel

    )# fin sidebarlayout
  )# fin fluidpage



croisementsInference<-

  fluidPage(

    navbarPage(id="Panel 2.x",title = NULL,

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
                                         id="datasetlogit",
                                         tabPanel("Variables sélectionnées",
                                                  h3("Variables sélectionnées"),
                                                  p("Les variables sélectionnées pour la réalisation du test diagnostique sont:"),
                                                  tableOutput("mytableLOGIT1")
                                         ),
                                         tabPanel("Courbe ROC",
                                                  p("La courbe ROC réalisée à partir des variables sélectionnées est présentée ci-dessous"),
                                                  checkboxInput("LOGIToptionsGRAPHIQUES", "Je souhaite ajouter des options graphiques", FALSE),
                                                  conditionalPanel(
                                                    condition = "input.LOGIToptionsGRAPHIQUES",
                                                    checkboxInput("LOGIToptionsAUC", "Afficher Aire sous la courbe sur le graphique", FALSE),
                                                    checkboxInput("LOGIToptionsSEUIL", "Afficher seuil optimal sur le graphique", FALSE),
                                                    checkboxInput("LOGIToptionsIntervalle", "Afficher intervalle de confiance courbe ROC", FALSE)
                                                  ),
                                                  h3("Courbe ROC associée"),
                                                  plotOutput("LogitROC"),
                                                  br(),br(),
                                                  h3("Meilleur seuil estimé par maximisation de l'indice de Youden"),
                                                  tableOutput("LogitROCtableauBEST"),
                                                  br(),br(),
                                                  h3("Détails des seuils utilisés pour construire la courbe"),
                                                  tableOutput("LogitROCtableau")

                                         ), # fin tab panel
                                         tabPanel("Performances diagnostiques",
                                                  h4("Le meilleur seuil (au sens défini précédemment) est estimé à:"),
                                                  tableOutput("LogitPERFtableauBEST"),br(),
                                                  h4("Pour un tel seuil le tableau croisé devient:"),
                                                  tableOutput("LogitPERF3"),br(),
                                                  h4("La sensibilité et la specificité sont:"),
                                                  tableOutput("LogitPERF1"),br(),
                                                  h4("La critères de performances sont alors"),
                                                  tableOutput("LogitPERF2"),
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



concordanceAvecBase<-

  fluidPage(

    titlePanel("Analyse de concordance entre 2 lecteurs"),
    sidebarLayout(
      sidebarPanel(
        # quel type de saisie souhaité
        checkboxInput("CONCORsaisie", "Saisir les données des deux lecteurs manuellement"),
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

        # ajout de l'intervalle de confiance
        checkboxInput("CONCORinter", "Ajouter l'intervalle de confiance, le calcul peut prendre plusieurs minutes"),
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
                      downloadButton("PDFconcordance",label="AIDE et Détails",class = "butt"),
                      h4("Faites attention s'il y a un filtre")
          )
        ),#finFluidRow

        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        h3("Tableau croisé"),
        p("On présente ci-dessous le tableau croisé des lectures réalisées:"),
        tableOutput("mytableCONCORDANCE1"),br(),



        conditionalPanel(
          condition = "input.CONCORinter == true",
          verbatimTextOutput ("ConcordanceManuelleINTERV")
        ),# fin condi 1
        conditionalPanel(
          condition = "input.CONCORinter == false",
          verbatimTextOutput ("ConcordanceManuelleSimple")
        ),# fin condi 2
        p("Landis et Koch proposent l'interprétation suivante du coefficient Kappa de Cohen:"),
        tableOutput("LandisEtKoch2")



      )# fin MainPanel

    )# fin sidebarlayout
  )# fin fluidpage




concordanceSansBase<-

  fluidPage(

    titlePanel("Analyse de concordance entre 2 lecteurs"),
    sidebarLayout(
      sidebarPanel(
        p("Entrez les valeurs du lecteur 1, séparées par un espace"),
        textInput("Concoman1", label = "Valeurs du lecteur 1", value = ""),
        p("Entrez les valeurs du lecteur 2, séparées par un espace"),
        textInput("Concoman2", label = "Valeurs du lecteur 2", value = ""),
        checkboxInput("CONCORinter", "Ajouter l'intervalle de confiance (simulations, le calcul peut prendre plusieurs minutes)")       # si saisie manuelle


      ),# fin sidebar panel

      mainPanel(
        fluidRow(
          splitLayout(cellWidths = c("30%","70%"),
                      downloadButton("PDFconcordance",label="AIDE et Détails",class = "butt"),
                      h4("Faites attention s'il y a un filtre")
          )
        ),#finFluidRow

        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        h3("Tableau croisé"),
        p("On présente ci-dessous le tableau croisé des lectures réalisées:"),
        tableOutput("mytableCONCORDANCE2"),br(),



        conditionalPanel(
          condition = "input.CONCORinter == true",
          verbatimTextOutput ("ConcordanceManuelleINTERV2")
        ),



        conditionalPanel(
          condition = "input.CONCORinter == false",
          verbatimTextOutput ("ConcordanceManuelleSimple2")
        ),# fin condi 2
        p("Landis et Koch proposent l'interprétation suivante du coefficient Kappa de Cohen:"),
        tableOutput("LandisEtKoch2")



      )# fin MainPanel

    )# fin sidebarlayout
  )# fin fluidpage


