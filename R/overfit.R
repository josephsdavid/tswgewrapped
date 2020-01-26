#' overfit function
#' produces an overfit table
#' @param xs a time series
#' @param p the order of overfitting
#' @param type the type of estimate to make, defaults to burg
#' @return nothing, simply displays the factor table
#' @export
#' @examples
#' test <- generate(aruma, 400, s = 4)
#' overfit(test,20)
overfit <- function(xs, p, type = "burg") {
  # simply displays the factor table, tossing away the rest
  res <- tswge::est.ar.wge(xs, p, type = type)
}
