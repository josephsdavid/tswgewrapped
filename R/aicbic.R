aics <- list(
             aic <- function(x,p = 0:8, q = 0:5,silent) aic5(x,p, q, type = "aic", silent),
             bic <- function(x,p = 0:8, q = 0:5,silent) aic5(x, p, q, type = "bic", silent)
)

#' aicbic function
#' returns the top 5 aic and bic values for an ARMA time series
#' @param vec the vector (time series object) to operate on
#' @param p integer vector of the ar order
#' @param q integer vector of the ma order
#' @param parallel whether or not to run in parallel
#' @param cl the cluster in which to run on
#' @param silent whether or not to run silently
#' @return a list of data frames of top 5 aic and bic
#' @export
#' @examples
#' xs <- playground(200)
#' aicbic(xs)
aicbic <- function(vec, p = 0:8, q = 0:5, parallel = FALSE, cl = NULL, silent = FALSE){
  if(parallel == TRUE){
    parLapply(cl, aics, function(f) f(vec, p, q, silent = TRUE))
  } else {
    lapply(aics, function(f) f(vec, p, q,silent))
  }
}
