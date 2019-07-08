# TSWGE, wrapped
This package contains several convenient wrappers for tswge, useful for Dr. Sadler's time series class

## Installation

```sh
git clone https://github.com/josephsdavid/tswgewrapped.git
R CMD INSTALL tswgewrapped
```

or

```r
devtools::install_github("josephsdavid/tswgewrapped")
```

## Usage
Currently we have wrappers for arma, arima, aruma, sigplusnoise generators and forecasters

### Time series generation and forecasting
the `generate(type, ...)` function generates time series, while the `forecast(type, ...)` function forecasts

```r
library(tswgewrapped)

armats <- generate(arma, 100, phi = 0.2, theta = 0.4, plot = F)
armafore <- forecast(type = arma, x = armats, phi = 0.2, theta = 0.4, n.ahead = 20)

sznlts <- generate(aruma, n = 100, phi = -.9, s = 12)
sznlfore <- forecast(aruma, sznlts, phi = -.7, s = 12, n.ahead = 20)

arimats <- generate(arima, n = 100, d = 4)
arimafore <- forecast(arima, arimats, d = 4, n.ahead = 20)
```

### Time series transformtion

We can also transform seasonal and arima time series with the `transform(type, x, n)` function:

```r
no_more_seasons <- transform(seasonal, sznlts, 12)
no_more_wandering <- transform(arima, arimats, 4)
```

Note that transform can accept either strings or plain words as the type argument, and accepts "arima", "Arima", "ARIMA", "Aruma", "ARUMA", "aruma", "seasonal", "Seasonal" as possible values. 
It is also important to note that when transforming arima data more than one time (n > 1), it will output the plots for each transformation step. This is on purpose! Part of good, consistent time series analysis is to examine these plots.
