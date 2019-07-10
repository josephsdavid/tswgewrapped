
#' assessment function!
#' assess a time series
#' @export
assess <- function(...,x, n.ahead) {
	bcast <- forecast(..., x = x, lastn = T,  n.ahead = n.ahead)
	ASE  <- ase(x, bcast)
	return(ASE)
}
