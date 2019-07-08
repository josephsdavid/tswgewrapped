#' forecast function!
#' @export
#' @examples
#' forecast(arma, LakeHuron, phi = 0.2)

forecast <- function(type,...){
	a <- paste0("fore.", enexpr(type),".wge")
	fn <- parse_expr(a)
	fun <- expr((!!fn)(...))
	eval(fun)
}
