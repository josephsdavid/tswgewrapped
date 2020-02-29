#' Prints the factor table for a pure seasonal model (p = d = q = 0)
#' @param s Seasonality of the model
#' @export
factor.wge.season = function(s){
  phi = c(rep(0,s-1), 1)
  cat("--------------------------------------------------\n")
  cat(paste0("Printing Factors for Seasonality 's' = ", s, "\n"))
  cat("--------------------------------------------------\n")
  tswge::factor.wge(phi = phi)
}