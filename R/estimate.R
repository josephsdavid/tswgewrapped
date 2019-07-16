#' paramater estimation function
#' @export
estimate <- function(xs, p, q = 0, type = 'mle', ...) {
	if(q > 0) {
		return(est.arma.wge(xs, p, q,  ...))
	}
	else {
		return(est.ar.wge(xs, p, type, ...))
	}
}
