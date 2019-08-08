#' paramater estimation function
#' @param xs the time series
#' @param p the ar order (defaults to 0)
#' @param type the method of estimation, defaults to maximum likelihood
#' @return a list of estimates
#' @export
estimate <- function(xs, p, q = 0, type = 'mle', ...) {
  if(q > 0) {
    return(est.arma.wge(xs, p, q,  ...))
  }
  else {
    return(est.ar.wge(xs, p, type, ...))
  }
}
