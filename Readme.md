# TSWGE, wrapped
This package contains several convenient wrappers for tswge, useful for Dr. Sadler's time series class

## Installation

```sh
git clone https://github.com/josephsdavid/tswgewrapped.git
R CMD INSTALL tswgewrapped
```

## Usage
Currently we have wrappers for arma, arima, aruma, sigplusnoise generators and forecasters

### Time series generation and forecasting
the `gen_ts(type, ...)` function generates time series, while the `forecast(type, ...)` function forecasts

```r
library(tswgewrapped)

armats <- gen_ts(arma, 100, phi = 0.2, theta = 0.4, plot = F)
armafore <- forecast(type = arma, x = armats, phi = 0.2, theta = 0.4, n.ahead = 20)
```


