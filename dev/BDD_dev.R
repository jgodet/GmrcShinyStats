# Values: 

library(shiny)
BDD<- mtcars
r<-reactiveValues(
)
r$BASEchargee<-T
r$BDD<-BDD
r$noms<-colnames(BDD)
r$contentInput<-T
r$nbSujet<-dim(BDD)[1]
D <- BDD
Y<-as.data.frame(lapply(D, factor))
z<-as.numeric(lapply(Y, nlevels))
r$nbModeVariable<-z
r$variableNum<-as.logical(lapply(D, is.numeric))
a<-as.logical(lapply(D, is.numeric))
ret<-rep(NA, ncol(D))
ret[which(a)] <- as.logical(lapply(as.data.frame(D[,which(a)]), desctable::is.normal))
r$variableNormale<-ret
r$listeVariableNonNormale<-names(BDD)[!ret]
r$listeVariableNormale<-names(BDD)[ret]


 source("R/fct_code_sans_dep.R")
source("R/fct_fonctions.R")

library(gmrcfun)
ui <- shiny::fluidPage(mod_Croisements_ui(1))
source("R/mod_Croisements.R")
server <- function(input, output, session) {
  
  mod_Croisements_server(1,r=r)
}


shinyApp(ui, server)


