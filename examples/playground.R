library(tswgewrapped)
set.seed(38)
xs <- playground(400)
plotts.sample.wge(xs)
library(magrittr)
xs %>% difference(arima, x = ., 2)  -> xs_notrend
xs_notrend %>% difference(seasonal, ., 9)  -> xs_station
aicbic(xs_station)
est <- est.arma.wge(xs_station, p = 3, q = 1)
est$phi
est$theta
forecast(aruma, x = xs,d = 2, s = 9, phi = est$phi, theta = est$theta, n.ahead = 60)
assess(aruma, x = xs,d = 2, s = 9, phi = est$phi, theta = est$theta, n.ahead = 60)
