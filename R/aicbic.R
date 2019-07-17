aics <- list(
	     aic <- function(x,p = 0:8, q = 0:5) aic5.wge(x,p, q),
	     bic <- function(x,p = 0:8, q = 0:5) aic5.wge(x, p, q, type = "bic")
)

#' aicbic function
#' returns the top 5 aic and bic values for an ARMA time series
#' @param type either a string or plain text, defaults to "both", other values include AIC and BIC
#' @export
aicbic <- function(vec, p = 0:8, q = 0:5, parallel = FALSE, cl = NULL){
	if(parallel == TRUE){
		parLapply(cl, aics, function(f) f(vec, p, q))
	}
	else {
		lapply(aics, function(f) f(vec, p, q))
	}
}
