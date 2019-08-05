# file.choose2.r
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


file.choose2 <- function(...) {
  pathname <- NULL;
  tryCatch({
    pathname <- file.choose(T);
  }, error = function(ex) {
  })
  pathname;
}
