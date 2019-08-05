# tests_autoGMRC.r
# written by JuG
# August 05 2019


#' Do something
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'var=rnorm(100)
#'grp <- rep(letters[1:2],each=50)
#'tests_autoGMRC(var = var,grp = grp )
#'
#'var=rnorm(30)
#'grp <- rep(letters[1:3],each=10)
#'tests_autoGMRC(var = var,grp = grp )
#' @return
#' @export


tests_autoGMRC<-function (var, grp){
  grp <- grp %>% factor
  if (nlevels(grp) < 2){
    ~no.test
  }else if(var %>% is.factor){
    if ( tryCatch(stats::chisq.test(var , grp)$p.value>=0 , warning = function(e) F, error = function(e) F)){
      ~ chisq.test
    }else{
      ~fisher.test}
  }else{
    all_normal <- all(tapply(X = var,INDEX =  grp,FUN =  desctable::is.normal))
    if (nlevels(grp) == 2){
      if (all_normal){
        if (tryCatch(stats::var.test(var ~ grp)$p.value >
                     0.1, warning = function(e) F, error = function(e) F)){
          ~t.testVarEgal
        }else{
          ~. %>% t.test(var.equal = F)}
      }else{
        ~wilcox.test}
    }else if(all_normal){
      if (tryCatch(stats::bartlett.test(var ~ grp)$p.value >
                   0.1, warning = function(e) F, error = function(e) F))
        ~. %>% oneway.test(var.equal = T)
      else ~. %>% oneway.test(var.equal = F)
    }else{
      ~kruskal.test}
  }
}

