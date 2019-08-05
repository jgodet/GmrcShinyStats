# t.testVarEgal.r
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


t.testVarEgal<- function(x,...){
  t.test(x,var.equal = T,...)
}
