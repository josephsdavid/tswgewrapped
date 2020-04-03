## Load Libraries
library(testthat)
library(tswge)
library(tswgewrapped)
# library(tseries)

file = system.file("extdata", "USeconomic.csv", package = "tswgewrapped", mustWork = TRUE)
USeconomic = read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
names(USeconomic) = gsub("[(|)]", "", colnames(USeconomic))
# colnames(USeconomic) = c("logM1", "logGNP", "rs", "rl")

## Load Datasets
data("airlog")
data("AirPassengers")
data("sunspot.classic")
# data("USeconomic")

## Perform Checks
test_check("tswgewrapped")