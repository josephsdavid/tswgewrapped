#' plot the residuals of a time series
#' @export
plot_res <- function(res) {
	par(mfrow=c(2,1))
	plot(res, type = "b")
	acf(res)
	par(mfrow = c(1,1))
}
