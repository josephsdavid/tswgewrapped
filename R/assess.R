
#' assessment function!
#' assess a time series
#' @export
assess <- function(x, ...) {
	bcast <- forecast(x = x,..., lastn = T)
	ASE  <- ase(x, bcast)
	return(ASE)
}
