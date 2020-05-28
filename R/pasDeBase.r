# pasDeBase.r
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


pasDeBase <-   function(){
  a <- fluidPage(
  h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible" ),
  p("Pour charger une base de données, rendez-vous sur l'onglet Base de Données en haut de cette page.")
)
  return(a)
}
