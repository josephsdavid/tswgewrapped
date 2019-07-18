#' ljung box test for white noise
#' @param x the time series
#' @param p the ar order
#' @param q the ma order
#' @param k_val a vector of k values
#' @return the results of the models, in list format
#' @export
ljung_box <- function(x, p, q, k_val = c(24,48)) {
	ljung <- function(k) {
		ljung.wge(x = x, p = p, q = q, K = k)
	}
	sapply(k_val, ljung)
}
