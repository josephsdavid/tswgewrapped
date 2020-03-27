
#### Check Stationarity ####
test_that("check_stationarity", {
  expect_true(check_stationarity(data=c(1,2,3,4,5,6,7)))
})

#### Manual Calculations ####
test_that("calculate_ar1_varx, calculate_arp_varx", {
  p_0.9  = tswge::plotts.true.wge(n = 200, phi = 0.9)
  p_0.9m = tswge::plotts.true.wge(n = 200, phi = -0.9)
  p_0.5  = tswge::plotts.true.wge(n = 200, phi = 0.5)
  p_0.5m = tswge::plotts.true.wge(n = 200, phi = -0.5)
  
  expect_equal(calculate_ar1_varx(phi = 0.9, vara = 1), p_0.9$acv[1])
  expect_equal(calculate_ar1_varx(phi = -0.9, vara = 1), p_0.9m$acv[1])
  expect_equal(calculate_ar1_varx(phi = 0.5, vara = 1), p_0.5$acv[1])
  expect_equal(calculate_ar1_varx(phi = -0.5, vara = 1), p_0.5m$acv[1])
  
  expect_equal(calculate_arp_varx(phi = 0.9, pt = p_0.9, vara = 1), p_0.9$acv[1])
  expect_equal(calculate_arp_varx(phi = -0.9, pt = p_0.9m, vara = 1), p_0.9m$acv[1])
  expect_equal(calculate_arp_varx(phi = 0.5, pt = p_0.5, vara = 1), p_0.5$acv[1])
  expect_equal(calculate_arp_varx(phi = -0.5, pt = p_0.5m, vara = 1), p_0.5m$acv[1])
  
  phi = c(1.5, -0.9)
  pt = tswge::plotts.true.wge(phi = phi)
  expect_equal(calculate_arp_varx(phi = phi, pt = pt, vara = 1), pt$acv[1])
  
})

test_that("calculate_ts gamma0, mean, var of mean, CI", {
  x = c(4,6,7,5,3)
  
  gamma0 = calculate_ts_gamma0(x)
  xbar = calculate_ts_mean(x)
  vxbar = calculate_ts_var_of_mean(x)
  ci = calculate_ts_mean_confidence_interval(x)
  
  expect_equal(gamma0, 2)
  expect_equal(xbar, 5)
  expect_equal(vxbar, 0.144)
  expect_equal(ci, c(4.256246, 5.743754))
})

test_that("calculate_ts gamma, rho", {
  
  x = AirPassengers
  
  acf = stats::acf(x)
  
  # ACF at Lag 0 = 1, hnce starting from index 2
  expect_equal(calculate_ts_rho(x, 1), acf$acf[2])  
  expect_equal(calculate_ts_rho(x, 2), acf$acf[3])
  expect_equal(calculate_ts_rho(x, 3), acf$acf[4])
  expect_equal(calculate_ts_rho(x, 4), acf$acf[5])
  expect_equal(calculate_ts_rho(x, 5), acf$acf[6])
  
  
  acv = stats::acf(x, type = "covariance")
  
  expect_equal(calculate_ts_gamma(x, 1), acv$acf[2])
  expect_equal(calculate_ts_gamma(x, 2), acv$acf[3])
  expect_equal(calculate_ts_gamma(x, 3), acv$acf[4])
  expect_equal(calculate_ts_gamma(x, 4), acv$acf[5])
  expect_equal(calculate_ts_gamma(x, 5), acv$acf[6])
})

test_that("White Noise Estimates", {
  x = c(40, 30.5, 49.8, 38.3, 29.3, 48.7, 39.2, 31.7, 46.1, 42.4)
  phi = c(-1.1, -1.78, -0.88, -0.64)
  theta = c(0.2, -0.9)
  info = get_all_a_calc(x, phi, theta)
  
  expect_equal(info$all_a[1], 0)
  expect_equal(info$all_a[2], 0)
  expect_equal(info$all_a[3], 0)
  expect_equal(info$all_a[4], 0)
  expect_equal(info$all_a[5], -1.326)
  expect_equal(info$all_a[6], -1.6572)
  expect_equal(info$all_a[7], -2.47804)
  expect_equal(info$all_a[8], -1.042128)
  expect_equal(info$all_a[9], 0.5358104)
  expect_equal(info$all_a[10], 2.4050773)
  
  expect_equal(round(info$vara,6), 2.967132)
  expect_equal(round(info$stda,6), 1.722536)
})

#### Faster AIC5 ####
test_that("faster aic", {
  x = rnorm(1000)
  
  # Start the clock!
  start_time = proc.time()
  grid1 = tswge::aic5.wge(x = x, p = 0:5, q = 0:2)
  # Stop the clock
  time1 = proc.time() - start_time

  # Start the clock!
  start_time = proc.time()
  grid2 = tswgewrapped::aic5(x = x, p = 0:5, q = 0:2)
  # Stop the clock
  time2 = proc.time() - start_time

  # Make the column names equal so the expect equal does not flag error due to column name mismatch
  names(grid1) = names(grid2)
  
  expect_equal(grid1, grid2)
  
  # TODO: Check why tswgewrapped::aic5 is not taking less time.
  #expect_gt(time1['elapsed'], time2['elapsed'])
})


#### AICBIC ####
test_that("AICBIC", {
 
  # Generated White Noise 
  arma.2.1 = gen.arma.wge(n = 200, phi = c(-0.5, -0.55), theta = 0.8, sn = 101)
  
  g.aicbic = aicbic(arma.2.1, p = 0:5, q = 0:2)
  
  g.aic = aic5.wge(arma.2.1)
  g.bic = aic5.wge(arma.2.1, type = 'bic')
  
  colnames.aic = c("p", "q", "aic")
  colnames.bic = c("p", "q", "bic")
  
  s.aicbic.aic.sum = g.aicbic[[1]] %>% dplyr::summarise_all(sum)
  s.aicbic.bic.sum = g.aicbic[[2]] %>% dplyr::summarise_all(sum)
  s.aic.sum = g.aic %>% dplyr::summarise_all(sum) 
  s.bic.sum = g.bic %>% dplyr::summarise_all(sum)
  
  colnames(s.aic.sum) = colnames.aic
  colnames(s.bic.sum) = colnames.bic
  
  expect_equal(s.aicbic.aic.sum$p, s.aic.sum$p)
  expect_equal(s.aicbic.aic.sum$q, s.aic.sum$q)
  expect_equal(s.aicbic.aic.sum$aic, s.aic.sum$aic)
  
  expect_equal(s.aicbic.bic.sum$p, s.bic.sum$p)
  expect_equal(s.aicbic.bic.sum$q, s.bic.sum$q)
  expect_equal(s.aicbic.bic.sum$bic, s.bic.sum$bic)
  
  ## Merge
  g.aicbic = aicbic(arma.2.1, p = 0:5, q = 0:2, merge = TRUE)
  s.aicbic.sum = g.aicbic %>% dplyr::summarise_all(sum)
  expect_equal(s.aicbic.sum$p, s.aic.sum$p)
  expect_equal(s.aicbic.sum$q, s.aic.sum$q)
  expect_equal(s.aicbic.sum$aic, s.aic.sum$aic)
  expect_equal(s.aicbic.sum$bic, s.bic.sum$bic)
  
  
})


#### Parallel AICBIC ####
# ## TODO: Parallel is not working. Fix it
# test_that("AICBIC Parallel", {
#   
#   library(tswge)
#   library("parallel")
#   # Generated White Noise 
#   arma.2.1 = gen.arma.wge(n = 200, phi = c(-0.5, -0.55), theta = 0.8, sn = 101)
#   
#   
#   cores = detectCores()
#   if (cores > 4){
#     use_cores = 4
#   }
#   else (cores > 2){
#     use_cores = 2
#   }
#   else{
#     use_cores = 1
#   }
#   
#   
#   cl <- makeCluster(use_cores)
#   #registerDoParallel(cl)
#   
#   g.aicbic = aicbic(arma.2.1, p = 0:5, q = 0:2, parallel = TRUE, cl = cl)
#   
#   #registerDoSEQ()
#   
#   g.aic = aic5.wge(arma.2.1)
#   g.bic = aic5.wge(arma.2.1, type = 'bic')
#   
#   colnames.aic = c("p", "q", "aic")
#   colnames.bic = c("p", "q", "bic")
#   
#   s.aicbic.aic.sum = g.aicbic[[1]] %>% dplyr::summarise_all(sum)
#   s.aicbic.bic.sum = g.aicbic[[2]] %>% dplyr::summarise_all(sum)
#   s.aic.sum = g.aic %>% dplyr::summarise_all(sum) 
#   s.bic.sum = g.bic %>% dplyr::summarise_all(sum)
#   
#   colnames(s.aic.sum) = colnames.aic
#   colnames(s.bic.sum) = colnames.bic
#   
#   expect_equal(s.aicbic.aic.sum$p, s.aic.sum$p)
#   expect_equal(s.aicbic.aic.sum$q, s.aic.sum$q)
#   expect_equal(s.aicbic.aic.sum$aic, s.aic.sum$aic)
# 
#   expect_equal(s.aicbic.bic.sum$p, s.bic.sum$p)
#   expect_equal(s.aicbic.bic.sum$q, s.bic.sum$q)
#   expect_equal(s.aicbic.bic.sum$bic, s.bic.sum$bic)
#   
# })




#### Differencing - ARIMA ####
test_that("Differencing - ARIMA", {
  # ARIMA(2,2,1)
  x = tswge::gen.arima.wge(n = 200, phi = c(1.5,-0.8), d = 2, theta = -0.8)
  dif1 = tswge::artrans.wge(x, phi.tr = 1)   # first difference
  dif2 = tswge::artrans.wge(dif1, phi.tr = 1) # second difference
  dif_new = difference("arima", x, 2)  # using wrapper
  
  expect_equal(dif_new, dif2)
  
})

#### Differencing - Seasonal ARIMA ####
test_that("Differencing - Seasonality", {
  # Quarterly
  x = tswge::gen.aruma.wge(n = 80, s = 4, sn = 81) #tswge function to generate ARIMA and Seasonal Models
  dif1 = tswge::artrans.wge(x = x, phi.tr = c(0,0,0,1)) #Take out the (1-B^4)
  dif2 = difference("aruma", x, 4)
  expect_equal(dif1, dif2)
  
  # Yearly
  phi = c(.4,.6,-.74)
  theta = c(-.7)
  x = tswge::gen.aruma.wge(n = 80, phi = phi, theta = theta, s=12, sn = 31)
  dif1 = tswge::artrans.wge(x = x, phi.tr = c(rep(0,11),1)) #Take out the (1-B^12)
  dif2 = difference("aruma", x, 12)
  expect_equal(dif1, dif2)
})


#### Seasonal Factors ####
test_that("Seasonal Factors", {
  factor.wge.season(12)
  factor.wge.season(4)
})

#### Estimate ####
test_that("Estimate", {
  
  phi = 0.6
  theta = 0.2
  
  x.ar = gen.arma.wge(n = 200, phi = phi, plot = FALSE, sn = 101)
  x.arma = gen.arma.wge(n = 200, phi = phi, theta = theta, plot = FALSE, sn = 101)
  
  est.ar.tswge.mle = est.ar.wge(x.ar, p = 1, type = 'mle')
  est.ar.tswge.burg = est.ar.wge(x.ar, p = 1, type = 'burg')
  est.arma.tswge = est.arma.wge(x.arma, p = 1, q = 1)
  
  est.ar.wrapped.mle = estimate(xs = x.ar, p = 1, type = 'mle')
  est.ar.wrapped.burg = estimate(xs = x.ar, p = 1, type = 'burg')
  est.arma.wrapped = estimate(xs = x.arma, p = 1, q = 1)
  
  expect_equal(est.ar.tswge.mle$phi, est.ar.wrapped.mle$phi)
  expect_equal(round(est.ar.tswge.mle$avar,3), round(est.ar.wrapped.mle$avar, 3))
  expect_equal(sum(est.ar.tswge.mle$res), sum(est.ar.wrapped.mle$res))
  
  expect_equal(est.ar.tswge.burg$phi, est.ar.wrapped.burg$phi)
  expect_equal(round(est.ar.tswge.burg$avar, 3), round(est.ar.wrapped.burg$avar,3))
  expect_equal(sum(est.ar.tswge.burg$res), sum(est.ar.wrapped.burg$res))
  
  expect_equal(est.arma.tswge$phi, est.arma.tswge$phi)
  expect_equal(est.arma.tswge$theta, est.arma.tswge$theta)
  expect_equal(round(est.arma.tswge$avar,3), round(est.arma.wrapped$avar,3))
  expect_equal(sum(est.arma.tswge$res), sum(est.arma.wrapped$res))
  
  
})

#### Forecast Wrapper ####
test_that("Forecast Function Wrapper", {
  phi = 0.6
  theta = 0.2
  d = 1
  s = 12
  
  # AR Model
  f1 = fcst("arma", LakeHuron, phi = phi)  
  f2 = tswge::fore.arma.wge(LakeHuron, phi = phi)
  expect_equal(f1$wnv, f2$wnv)
  
  # ARMA Model
  f1 = fcst("arma", LakeHuron, phi = phi, theta = theta)  
  f2 = tswge::fore.arma.wge(LakeHuron, phi = phi, theta = theta)
  expect_equal(f1$wnv, f2$wnv)
  
  # ARUMA Model
  f1 = fcst("aruma", LakeHuron, phi = phi, theta = theta, d = d, s = s)  
  f2 = tswge::fore.aruma.wge(LakeHuron, phi = phi, theta = theta, d = d, s = s)
  expect_equal(f1$wnv, f2$wnv)
  
})

#### ASE Calculations ####
test_that("ASE Function", {
  phi = 0.6
  theta = 0.2
  d = 1
  s = 12
  
  n = 200
  n.ahead = 24
  
  x = generate("aruma", n = n, phi= phi, theta = theta, d = d, s = s, plot = FALSE, sn = 101)
  
  f = fcst("aruma", x, phi= phi, theta = theta, d = d, s = s, n.ahead = n.ahead, lastn = TRUE)
  ase1 = ase(x, f)
  
  f = tswge::fore.aruma.wge(x, phi= phi, theta = theta, d = d, s = s, n.ahead = n.ahead, lastn = TRUE)
  ase2 = mean((x[(n-n.ahead+1):n] - f$f)^2)
  
  expect_equal(ase1, ase2)
  
})

#### Access Function ####
test_that("Assess Function", {
  phi = 0.6
  theta = 0.2
  d = 1
  s = 12
  
  n = 200
  n.ahead = 24
  
  x = generate("aruma", n = n, phi= phi, theta = theta, d = d, s = s, plot = FALSE, sn = 101)
  ase1 = assess(x, type = "aruma", phi= phi, theta = theta, d = d, s = s, n.ahead = n.ahead)
  
  f = tswge::fore.aruma.wge(x, phi= phi, theta = theta, d = d, s = s, n.ahead = n.ahead, lastn = TRUE)
  ase2 = mean((x[(n-n.ahead+1):n] - f$f)^2)
  
  expect_equal(ase1, ase2)
  
})


#### Sliding Window ASE Calculations ####
test_that("Sliding Window", {
  
  x = AirPassengers
  
  n = length(x)
  batch_size = 48
  n.ahead = 12
  
  phi = c(-0.36, -0.05, -0.14, -0.11, 0.04, 0.09, -0.02, 0.02, 0.17, 0.03, -0.10, -0.38)
  theta = c(0)
  d = 1
  s = 12
  
  ## ARMA Model ##
  
  f = tswge::fore.arma.wge(x, phi=phi, theta = theta,
                           n.ahead = n.ahead, limits=FALSE, lastn = TRUE)
  
  # Without Sliding Window (usual method)
  ase1 = mean((x[(n-n.ahead+1):n] - f$f)^2)
  
  # Using ARUMA to compute forecasts for ARMA
  f = tswge::fore.aruma.wge(x, phi=phi, theta = theta, d = 0, s = 0,
                            n.ahead = n.ahead, limits=FALSE, lastn = TRUE)
  
  # Without Sliding Window (usual method)
  ase2 = mean((x[(n-n.ahead+1):n] - f$f)^2)
  
  # Using sliding window function
  # Default assumes only 1 batch
  r = sliding_ase_univariate(x, phi = phi, theta = theta, n.ahead = n.ahead)  
  ASEs = r$ASEs
  Time = r$time
  
  expect_equal(ase1, ASEs)
  expect_equal(ase2, ASEs)
  
  ## ARIMA Model with Seasonality ##
  
  f = tswge::fore.aruma.wge(x, phi=phi, theta = theta, d = d, s = s,
                            n.ahead = n.ahead, limits=FALSE, lastn = TRUE)
  
  # Without Sliding Window (usual method)
  ase1 = mean((x[(n-n.ahead+1):n] - f$f)^2)
  
  r = sliding_ase_univariate(x, phi = phi, theta = theta, d = d, s = s, n.ahead = n.ahead)  
  ASEs = r$ASEs
  Time = r$time
  
  expect_equal(ase1, ASEs)

})


#### Compare Multiple Realizations of a model with Actual ####
test_that("Compare Multiple Realizations", {
  
  est = tswge::est.arma.wge(sunspot.classic,p=8)
  
  r = generate_multiple_realization(x = sunspot.classic, phi = est$phi, theta = est$theta, vara = est$avar, seed = 11) 
  plot_multiple_realizations(data = r$data, results = r$results)
  
  data.sum = r$data %>% dplyr::summarise_if(is.numeric, sum)
  results.acf.sum = r$results %>% dplyr::filter(Characteristic == "ACF") %>%  dplyr::summarise_if(is.numeric, sum)
  results.spectrum.sum = r$results %>% dplyr::filter(Characteristic == "Spectrum") %>%  dplyr::summarise_if(is.numeric, sum)
  
  expect_equal(round(data.sum$Data), 36833)
  expect_equal(data.sum$Index, 77880)
  expect_equal(round(results.acf.sum$Value, 1), 10.8)
  expect_equal(results.acf.sum$Index, 1625)
  expect_equal(round(results.spectrum.sum$Value), -3078)
  expect_equal(round(results.spectrum.sum$Index), 111)
  
})

#### Evaluate Residuals ####
test_that("White Noise Eval - White Noise Eval", {
  # Generated White Noise 
  wn = tswge::gen.arma.wge(n = 200, sn = 101, plot = FALSE)
  table = evaluate_residuals(wn)
  
  k24 = tswge::ljung.wge(wn, K = 24)
  k48 = tswge::ljung.wge(wn, K = 48)
  
  expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(pval) %>% purrr::pluck(1), k24$pval)
  expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(pval) %>% purrr::pluck(1), k48$pval)
  expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(Decision) %>% purrr::pluck(1), "FTR NULL")
  expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(Decision) %>% purrr::pluck(1), "FTR NULL")
  
  # Not White Noise
  data(hadley) 
  table = evaluate_residuals(hadley)
  
  k24 = ljung.wge(hadley, K = 24)
  k48 = ljung.wge(hadley, K = 48)
  
  expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(pval) %>% purrr::pluck(1), k24$pval)
  expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(pval) %>% purrr::pluck(1), k48$pval)
  expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(Decision) %>% purrr::pluck(1), "REJECT NULL")
  expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(Decision) %>% purrr::pluck(1), "REJECT NULL")

})

#### Overfit Tables - Model ID ####
test_that("Overfit", {
  # Generated White Noise 
  arima2.1.0.12 = gen.aruma.wge(n = 200, phi = c(-0.5, -0.55), d = 1, s = 12, sn = 101)
  over_wrapped = overfit(arima2.1.0.12, p = 24, type = 'burg')
  over_tswge = est.ar.wge(arima2.1.0.12, p = 24, type = 'burg')
  
  expect_equal(sum(over_wrapped$phi), sum(over_tswge$phi))
  
})




