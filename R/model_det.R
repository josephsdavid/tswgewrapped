model_linear <- function(xs){
  t <- 1:length(xs)
  ts_lm <- stats::lm(xs ~ t)
  resids <- ts_lm$residuals
  pars <- tswge::aic.wge(resids, p = 0:6)
  xs_trans <- tswge::artrans.wge(xs, phi.tr = pars$phi)
  t_trans <- tswge::artrans.wge(t, phi.tr = pars$phi)
  model <- stats::lm(xs_trans ~ t_trans)
  tswge::plotts.sample.wge(model$resi)
  tswge::ljung.wge(model$resi)
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
  tswge::gen.sigplusnoise.wge(n = length(xs), b0 = lmod$intercept, b1 = lmod$slope, vara = lmod$vara, phi = lmod$phi, plot = FALSE)
  sat <- readline(prompt = "are you satisfied (y/n): ")
  if (sat == "y") return("great")
  else test_lm(xs,lmod)
}

predict_lm <- function(xs,lmod) {
  tswge::fore.sigplusnoise.wge(xs, max.p = lmod$p, n.ahead = 50, limits = FALSE)
  ASE <- assess(type = "sigplusnoise", x = xs, max.p = lmod$p, n.ahead = 50, limits = FALSE, plot = F)
  ASE
}

#' model with deterministic signal plus noise
#' @param xs the time series
#' @return the model parameters and metrics, as a list
#' @export
model_det <- function(xs){
  xslin <- model_linear(xs)
  test_lm(xs,xslin)
  ASE <- predict_lm(xs,xslin)
  xslin$ASE <- ASE
  xslin
}
