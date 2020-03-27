#' plot the residuals of a time series
#' @param res the residuals of a model
#' @return nothing, makes a plot
#' @export
plot_res <- function(res) {
  warning("This function has been depracated. Please use evaluate_residuals() instead.")
  # graphics::par(mfrow=c(2,1))
  # graphics::plot(res, type = "b")
  # stats::acf(res)
  # graphics::par(mfrow = c(1,1))
}
