
#' assessment function!
#' assess a time series
#' @export
assess <- function(...) {
	bcast <- forecast(..., lastn = T)
	ASE  <- ase(x, bcast)
	return(ASE)
}
