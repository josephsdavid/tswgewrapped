#' Model as MLR model
#' @param df The dataframe containing the time series data
#' @param formula Formula to be used for model creation
#' @export
#' @examples
#' model_mlr(mtcars, mpg~.)
#' model_mlr(mtcars, mpg~cyl + hp)
model_mlr <- function(df, formula) {
  vs <- all.vars(formula)
  dvar <- vs[1]
  ifelse(vs[2] == ".", 
         evar <- df[,!names(df) == dvar],
         evar <- df[,vs[ -1 ]])
  fit <- stats::lm(data = df, formula = formula)
  phi <- tswge::aic.wge(fit$resid, p = 0:8, q = 0:0)
  stats::arima(df[[dvar]], order = c(phi$p, 0,0), xreg =as.matrix(evar) )
}
