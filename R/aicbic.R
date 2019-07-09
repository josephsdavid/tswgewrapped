aics <- list(
	     aic <- function(x,...) aic5.wge(x,...),
	     bic <- function(x,...) aic5.wge(x, ...,type = "bic")
)
#' aicbic function
#' returns the top 5 aic and bic values for an ARMA time series
#' @param type either a string or plain text, defaults to "both", other values include AIC and BIC
#' @export
aicbic <- function(vec,...){
	lapply(aics, function(f) f(vec,...))
}
