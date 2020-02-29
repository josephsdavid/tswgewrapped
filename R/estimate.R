#' paramater estimation function
#' @param xs the time series
#' @param p the AR order
#' @param q the MA order (defaults to 0)
#' @param type the method of estimation, defaults to maximum likelihood
#' @param ... Additional arguments to the tswge::est.arma.wge function
#' @return a list of estimates
#' @export
estimate <- function(xs, p=NA, q = 0, type = 'mle', ...) {
  if(all(is.na(p))){
    stop("You need to specify the AR order for the model.")
  }
  if(q > 0) {
    return(tswge::est.arma.wge(xs, p, q,  ...))
  }
  else {
    return(tswge::est.ar.wge(xs, p, type, ...))
  }
}
