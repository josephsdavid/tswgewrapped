
#' generator function!
#' @export
#' @examples
#' gen_ts(arma, 100, phi = 0.2, theta = 0.4)

gen_ts <- function(type,...){
	a <- paste0("gen.", enexpr(type),".wge")
	fn <- parse_expr(a)
	eval(call2(fn,...))
}
