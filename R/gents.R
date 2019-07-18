
#' generator function!
#' @param type either arma, aruma, sigplusnoise, or other tswge model. No quotes
#' @param ... the normal inputs to tswge
#' @return a time series
#' @export
#' @examples
#' generate(arma, 100, phi = 0.2, theta = 0.4)

generate <- function(type,...){
	phrase <- paste0("gen.", enexpr(type),".wge")
	func <- parse_expr(phrase)
	eval(call2(func,...))
}
