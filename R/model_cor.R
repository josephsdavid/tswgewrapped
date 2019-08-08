d_test <- function(xs) {
  d_order <- as.numeric(readline(prompt = "enter an arima order: "))
  if(d_order > 0){
    res <- difference(arima, xs, d_order)
    return(list(d = d_order, result = res))
  } else { 
    res  <-  xs
    return(list(d = d_order, result = res))
  }

}
guess_d_order <- function(xs){
  plotts.sample.wge(xs)
  overfit(xs, 20)
  dguess <- d_test(xs)
  sat <- readline(prompt = "are you satisfied (y/n): ")
  if (sat == "y"){
    return(dguess)
  } else {
    guess_d_order(xs)
  }
}

s_test <- function(xs) {
  s_order <- as.numeric(readline(prompt = "enter a seasonal order: "))
  if(s_order > 0){
    res <- difference(seasonal, xs, s_order)
    return(list(s = s_order, result = res))
  } else { 
    res  <-  xs
    return(list(s = s_order, result = res))
  }

}
guess_s_order <- function(xs){
  plotts.sample.wge(xs)
  overfit(xs,20)
  sguess <- s_test(xs)
  sat <- readline(prompt = "are you satisfied (y/n): ")
  if (sat == "y"){
    sguess
  }
  else {
    guess_s_order(xs)
  }
}

printaicbic <- function(xs, s, parallel = FALSE, cl = NULL){
  if(s > 3) {
    porder <- s+5
  } else(porder <- 8)
  aicbic(xs, p = 0:porder, q = 0:8, parallel, cl)
}
get_pq_order <- function() {
  p <- as.numeric(readline(prompt = "enter a chosen value of p: "))
  q <- as.numeric(readline(prompt = "enter a chosen value of q: "))
  list(p = p, q = q)
}

#' Model Correlated data
#' Interactively build a model of correlated data
#' @param xs the time series to model
#' @param parallel whether or not to run in parallel, defaults to FALSE
#' @param cl the cluster in which to run on
#' @return a list of model metrics and paramaters
#' @export
#' @examples
#' xs <- playground(400)
#' clust <- makeCluster(2L, type = "FORK")
#' model_cor(xs, parallel = TRUE. cl = clust)
model_cor <- function(xs, parallel = FALSE, cl = NULL) {
  ds <- guess_d_order(xs)
  d  <- ds$d
  xsd <- ds$result
  ss <- guess_s_order(xsd)
  s <- ss$s
  xsds <- ss$result
  aics <- printaicbic(xsds, s, parallel, cl)
  pander(aics)
  pqs <- get_pq_order()
  p <- pqs$p
  q <- pqs$q
  est_xs <- estimate(xsds, p, q)
  if(q == 0) {
    est_xs$theta  <-  0
  }
  phi <- est_xs$phi
  theta <- est_xs$theta
  paste("phi =", phi,"\ntheta =", theta)
  plot_res(est_xs$res)
  lj <- ljung_box(est_xs$res, p, q)
  pander(lj)
  ahead <- as.numeric(readline(prompt = "how far ahead do you want to forecast? "))
  fore <- fcst(aruma, xs, phi = phi, theta = theta, s = s, d = d, n.ahead = ahead)
  ASE <- assess(type = aruma, x = xs, phi = phi, theta = theta, s = s, d = d, n.ahead = ahead, plot =FALSE)
  res <- list(
              s = s,
              d = d,
              p = p,
              q = q,
              phi = phi,
              theta = theta,
              vara = est_xs$avar,
              mu = mean(xs),
              ljung_box = lj,
              ase = ASE
  )
  return(res)
}
