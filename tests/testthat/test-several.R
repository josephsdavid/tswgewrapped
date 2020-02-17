test_that("check_stationarity", {
  expect_true(check_stationarity(x=c(1,2,3,4,5,6,7)))
})


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


# test_that("faster aic", {
#   # Start the clock!
#   start_time = proc.time()
#   grid1 = tswge::aic5.wge(x = rnorm(1000), p = 0:5, q = 0:2)
#   # Stop the clock
#   time1 = proc.time() - start_time
#   
#   # Start the clock!
#   start_time = proc.time()
#   grid2 = tswgewrapped::aic5(x = rnorm(1000), p = 0:5, q = 0:2)
#   # Stop the clock
#   time2 = proc.time() - start_time
#   
#   expect_equal(grid1, grid2)
#   expect_gt(time1['elapsed'], time2['elapsed'])
# })

test_that("Differencing - ARIMA", {
  # ARIMA(2,2,1)
  x = tswge::gen.arima.wge(n = 200, phi = c(1.5,-0.8), d = 2, theta = -0.8)
  dif1 = tswge::artrans.wge(x, phi.tr = 1)   # first difference
  dif2 = tswge::artrans.wge(dif1, phi.tr = 1) # second difference
  dif_new = difference("arima", x, 2)  # using wrapper
  
  expect_equal(dif_new, dif2)
  
})

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




