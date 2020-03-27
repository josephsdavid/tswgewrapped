## Load Libraries
library(testthat)
library(tswgewrapped)
library(tswge)
library(tseries)

## Load Datasets
data("airlog")
data("AirPassengers")
data("sunspot.classic")
data("USeconomic")

## Perform Checks
test_check("tswgewrapped")
