#' forecast function!
#' @param type either arma, aruma, sigplusnoise, or other tswge model. No quotes
#' @param ... the normal inputs to tswge
#' @export
#' @return a forecast
#' @examples
#' fcst(arma, LakeHuron, phi = 0.2)

fcst <- function(type,...){
  phrase <- paste0("tswge::fore.", rlang::enexpr(type),".wge")
  func <- rlang::parse_expr(phrase)
  eval(rlang::expr((!!func)(...)))
}
