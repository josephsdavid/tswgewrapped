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

or
```r
install.packages("remotes")
remotes::install_github("josephsdavid/tswgewrapped")
```

## Usage
Currently we have wrappers for arma, arima, aruma, sigplusnoise generators and forecasters

### Time series generation and forecasting
the `generate(type, ...)` function generates time series, while the `fcst(type, ...)` function forecasts

```r
library(tswgewrapped)

armats <- generate(arma, 100, phi = 0.2, theta = 0.4, plot = F)
armafore <- fcst(type = arma, x = armats, phi = 0.2, theta = 0.4, n.ahead = 20)

sznlts <- generate(aruma, n = 100, phi = -.9, s = 12)
sznlfore <- fcst(aruma, sznlts, phi = -.7, s = 12, n.ahead = 20)

arimats <- generate(arima, n = 100, d = 4)
arimafore <- fcst(arima, arimats, d = 4, n.ahead = 20)
```

### Time series transformtion

We can also transform seasonal and arima time series with the `difference(type, x, n)` function:

```r
no_more_seasons <- difference(seasonal, sznlts, 12)
no_more_wandering <- difference(arima, arimats, 4)
```

Note that difference can accept either strings or plain words as the type argument, and accepts "arima", "Arima", "ARIMA", "Aruma", "ARUMA", "aruma", "seasonal", "Seasonal" as possible values. 
It is also important to note that when transforming arima data more than one time (n > 1), it will output the plots for each transformation step. This is on purpose! Part of good, consistent time series analysis is to examine these plots.

### Model Identification

A lot of times, it is good to not only look at the aic, but also the BIC, of your data when identifying the ARMA order of a model. For this we have the `aicbic()` function, which runs both aic5 and bic5 with the same arguments, returning results in the form of a list, where [[1]] is aic and [[2]] is bic (hence the name aicbic).

```r
aicbic(no_more_seasons)
aicbic(no_more_wandering, p = 0:13, q = 0:5)
```

### Model assessment

We can calculate the ASE of a model with the `ase` function:

```r
sznback <- fcst(aruma, sznlts, s = 12, phi = -.9, n.ahead = 20, lastn = T)
ase(sznlts, sznback)
```

We can also use the assess function as a convenient wrapper for ASE, which backcasts the forecast and calculates the ASE all in one go
```r
assess(aruma, sznlts, s = 12, phi = -.9, n.ahead = 20)
```

## Bringing it all together
Here we present an example workflow for time series analysis, from model identification to forecasting:

```r
examplets <- generate(aruma, n = 500, s = 12, d = 3, phi = c(.2,.4,-.2), theta = c(-.6))
```

First we examine our data with either plotts.sample.wge or individual tswge plotting functions:

```r
plotts.sample.wge(examplets)

#> or

plotts.wge(examplets)
parzen.wge(examplets)
acf(examplets)
```

Next we have multiple tools to identify the order of the model: using Tiao-Tay overfitting and using Box-Jenkins-esque methods. For overfitting, we simply do est.ar.wge with a high value:

```r
estim <- est.ar.wge(examplets, p = 20)
```

However first we likely want to look for the arima order. We can do this the box jenkins method quickly with `difference`

```r
difference(arima, examplets, 5)
```

Since this recursively transforms, we can watch until the wandering component goes away, and set that value to be our order. We can also eliminate the seasonal component first too, with a fun guess and check technique

```r
lapply(1:20, difference, x = examplets, type = seasonal)
```

and pick which seasonal component best fits. These qualitative  combined with the quantitative Tiao-Tsay method of overfitting make up a complete, thourough, and well founded framework for determining the nonstationary orders of a time series, with a hopefully terse enough syntax that they will not tax the analyst more than they already are.  After applying either of these graphical methods to determine one, it is probably wise to overfit using the other, and then double check by doing assessing the other component qualitatively and overfit the first. Once the order is found, we can then quickly assess the ARMA order:

```r
library(magrittr)
szns <- 12
int_order <- 3
examplets %>% difference(arima, x = ., n = int_order) %>% 
	difference(seasonal, x = ., n = szns) -> transts

aics <- aicbic(transts, p = 0:20, q = 0:5)

library(pander)
pander(aics[[1]])
pander(aics[[2]])

pqs <- estimate(transts, p = 3, q = 1)
```

Next we can forecast ahead with our forecast function:

```r
fcst(aruma, examplets, phi = pqs$phi, 
	theta = pqs$theta, s = szns, d = int_order, n.ahead = 20)
```

Finally we can assess our forecast with the assess (ASE) function

```r
assess(type = aruma, x = examplets, phi = pqs$phi, 
	theta = pqs$theta, s = szns, d = int_order, n.ahead = 20)
```

## Random time series generation:
Generate a random time series to test yourself with `playground(n)`
```r
xs <- playground(400)
```

# Multivariate time series!


