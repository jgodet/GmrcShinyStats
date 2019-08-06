# tablePourcent.r
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


tablePourcent<- function(base){
  pourcent <-  prop.table(table(base))
  pourcent<- pourcent[order(pourcent)]
  data<- data.frame(pourcent = as.numeric(pourcent), nom = names(pourcent))
  return(data)
  }
