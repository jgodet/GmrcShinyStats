# diagrammeBarre.r
# written by JuG
# August 05 2019


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


diagrammeBarre <- function(base){
  data<- tablePourcent(base)
  bp<-ggplot(data=data, aes(x=nom ,y=pourcent*100, fill=reorder(nom, 1/pourcent)))

  maxPourcent<- max(data$pourcent, na.rm = T)
  label<-  paste(round(data$pourcent,3)*100,"%")
  vjust<- unlist(as.list(ifelse(data$pourcent< maxPourcent/5, -1.6, 1.6)), use.names = F)

  barre <- bp +
    labs(title="Diagramme en barre",
         x="", y = "pourcentage")+
    geom_bar(stat="identity", color='black')+
    guides(fill=guide_legend(override.aes=list(colour=NULL)))+


    geom_text(aes( label = label), vjust=vjust, color="black", size=5) +
    theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=17))

  barre$labels$fill <- ""
  return(barre)

}
