
#' Model as MLR model
#' @export
#' @examples
#' model_mlr(mtcars, mpg~.)
#' model_mlr(mtcars, mpg~cyl + hp)
model_mlr <- function(df, formula ) {
	vs <- all.vars(formula)
	dvar <- vs[1]
	ifelse(vs[2] == ".", 
		evar <- df[,!names(df) == dvar],
		evar <- df[,vs[ -1 ]])
	fit <- lm(data = df, formula = formula)
	phi <- aic.wge(fit$resid, p = 0:8, q = 0:0)
	arima(df[[dvar]], order = c(phi$p, 0,0), xreg =as.matrix(evar) )
}
