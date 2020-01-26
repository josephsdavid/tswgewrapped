#' assessment function!
#' assess a time series
#' @param x the time series
#' @param ... the normal arguments to the fcst function. NOTE: we must put x first or name all of our arguments. This will be fixed
#' @return the ASE
#' @export
assess <- function(x, ...) {
  bcast <- fcst(x = x,..., lastn = T)
  ASE  <- ase(x, bcast)
  return(ASE)
}
