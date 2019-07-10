library(tswgewrapped)
examplets <- generate(aruma, n = 500, s = 12, d = 2, phi = c(.2,.4,-.2), theta = c(-.6), sn = 69)
difference(arima,examplets,2)
difference(seasonal, examplets, 12)


library(magrittr)
plotts.sample.wge(examplets)

examplets %>% 
  difference(seasonal, ., 12)  %>% 
  difference(arima, ., 2) -> statts

aicbic(statts)


forecast(aruma, examplets, phi = c(0.2, 0.4, -.2), s = 12, theta = -.6, d = 2, n.ahead = 50)
# this gets the ASE!!!!
assess(aruma, examplets, phi = c(0.2, 0.4, -.2), s = 12, theta = -.6, d = 2, n.ahead = 50)
