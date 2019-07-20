expand <- function(v1,v2){
	c(sapply(v1, function (x) rep(x, length(v2)) ))
}

rewrite <- function(v1, v2){
	rep(v1, length(v2))
}
getpq <- function(x, p=8,q=5, type = "aic", silent = FALSE){
	if (silent == FALSE){
		cat("Calculating ",type," for ARMA(",p,", ", q," )\n", sep = "")
	}
	res <- try(aic.wge(x, p,q,type))
	if (is.list(res)) {
		out <- c(res$p, res$q, res$value)
	} else {
		out <- c(p,q,9999)
	}
	out
}

#' aic5: slightly faster model estimations
#' todo: rewrite backcast and aic.wge for real speed gains
#' returns the top 5 aic
#' @param vec the vector (time series object) to operate on
#' @param p integer vector of the ar order
#' @param q integer vector of the ma order
#' @param silent whether or not to return output
#' @return a data frame of top 5 models
#' @export
aic5 <- function(x, p = 0:8, q = 0:5, type= "aic", silent = FALSE){
	ip <- expand(p,q)
	iq <- rewrite(q,p)
	out <- mapply(function(v1,v2) getpq(x, v1, v2, type, silent), ip, iq)
	out <- (as.data.frame(t(out)))
	colnames(out) <- c("p","q",type)
	head(out[order(out[, 3], decreasing = F),], 5)

}

