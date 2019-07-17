model_linear <- function(xs){
	t <- 1:length(xs)
	ts_lm <- lm(xs ~ t)
	resids <- ts_lm$resi
	pars <- aic.wge(resids, p = 0:6)
	xs_trans <- artrans.wge(xs, phi.tr = pars$phi)
	t_trans <- artrans.wge(t, phi.tr = pars$phi)
	model <- lm(xs_trans ~ t_trans)
	plotts.sample.wge(model$resi)
	ljung.wge(model$resi)
	readline(prompt = "hit enter to proceed")
	intercept <- ts_lm$coef[[1]]
	slope <- ts_lm$coef[[2]]
	list(b0 = intercept,
	     b1 = slope,
	     p = pars$p,
	     vara = pars$vara,
	     phi = pars$phi
	)
}
test_lm <- function(xs,lmod){
	gen.sigplusnoise.wge(n = length(xs), b0 = lmod$intercept, b1 = lmod$slope, vara = lmod$vara, phi = lmod$phi, plot = FALSE)
	sat <- readline(prompt = "are you satisfied (y/n): ")
	if (sat == "y") return("great")
	else test_lm(xs,lmod)
}
predict_lm <- function(xs,lmod) {
	fore.sigplusnoise.wge(xs, max.p = lmod$p, n.ahead = 50, limits = FALSE)
	ASE <- assess(type = sigplusnoise, x = xs, max.p = lmod$p, n.ahead = 50, limits = FALSE, plot = F)
	ASE
}

#' model with deterministic signal plus noise
#' @export
model_det <- function(xs){
	xslin <- model_linear(xs)
	test_lm(xs,xslin)
	ASE <- predict_lm(xs,xslin)
	xslin$ASE <- ASE
	xslin
}
