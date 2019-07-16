#' ljung box test for white noise
#' @export
ljung_box <- function(x, p, q, k_val = c(24,48)) {
	ljung <- function(k) {
		ljung.wge(x = x, p = p, q = q, K = k)
	}
	sapply(k_val, ljung)
}
