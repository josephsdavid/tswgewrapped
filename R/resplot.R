#' plot the residuals of a time series
#' @param res the residuals of a model
#' @return nothing, makes a plot
#' @export
plot_res <- function(res) {
	par(mfrow=c(2,1))
	plot(res, type = "b")
	acf(res)
	par(mfrow = c(1,1))
}
