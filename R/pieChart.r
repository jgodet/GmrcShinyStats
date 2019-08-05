# pieChart.r
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


pieChart<- function(base){
  data<- tablePourcent(base)
  bp<- ggplot(data=data, aes(x=0 ,y=pourcent, fill=reorder(nom, 1/pourcent)))+
    coord_polar(theta='y')
  df <- try(data %>% mutate(pos = cumsum(sort(data$pourcent))- sort(data$pourcent)/2))

  if(length(df)>0){
    label <- (sort(round(data$pourcent*100,1)))
    label <- as.character(ifelse(label<4,"",paste(label ,"%")))

    nom <- data$nom[order(data$pourcent,decreasing = F)]

    y<- df$pos

    pie <- bp +
      labs(title="Diagramme circulaire",
           x="", y = "")+
      theme(axis.text.x=element_blank())+
      geom_bar(stat="identity", color='black')+
      guides(fill=guide_legend(override.aes=list(colour="black")))+
      theme(axis.ticks=element_blank(),
            axis.title=element_blank(),
            axis.text.y=element_blank()) +

      theme_void()+
      geom_text(aes( x= 0.2,y=df$pos, label = label), size=6) +
      theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=17))


    pie$labels$fill <- ""
    pie$theme$legend.title$size <- 15
    return(pie)

  }else{
    return("une erreur c'est produite")
  }
}
