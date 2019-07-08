
#' generator function!
#' @export
#' @examples
#' generate(arma, 100, phi = 0.2, theta = 0.4)

generate <- function(type,...){
	phrase <- paste0("gen.", enexpr(type),".wge")
	func <- parse_expr(phrase)
	eval(call2(func,...))
}
