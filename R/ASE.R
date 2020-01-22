#' ASE calculator
#' @param x the time series
#' @param xhat the saved forecast (results of forecast function)
#' @return the ASE
#' @export
#' @examples
#' xs <- generate(arma, 200)
#' fore <- fcst(arma, xs, n.ahead =20, lastn = TRUE)
#' ase(xs, fore)

ase <- function(x, xhat){
  s <- length(x) - length(xhat$f) + 1
  n <- length(x)
  mean((xhat$f-x[s:n])^2)
}
