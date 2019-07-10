
#' assessment function!
#' assess a time series
#' @export
assess <- function(x, ..., n.ahead) {
	bcast <- forecast(..., lastn = T, x = x, n.ahead = n.ahead)
	ASE  <- ase(x, bcast)
	return(ASE)
}
