#' ASE calculator
#' @param x the time series
#' @param xhat the saved forecast (results of forecast function)
#' @export

ASE <- function(x, xhat){
	xi <- x[(length(x) - length(xhat$f)) : length(x)]
	error <- xhat$f - xi
	squarror <- error^2
	mean(squarror)
}
