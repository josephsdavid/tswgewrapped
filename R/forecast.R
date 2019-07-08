#' forecast function!
#' @export
#' @examples
#' forecast(arma, LakeHuron, phi = 0.2)

forecast <- function(type,...){
	phrase <- paste0("fore.", enexpr(type),".wge")
	func <- parse_expr(phrase)
	eval(expr((!!func)(...)))
}
