## Load Libraries
library(testthat)
library(tswge)
library(tswgewrapped)

file = system.file("extdata", "USeconomic.csv", package = "tswgewrapped", mustWork = TRUE)
USeconomic = read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
names(USeconomic) = gsub("[(|)]", "", colnames(USeconomic))

## Load Datasets
data("airlog")
data("AirPassengers")
data("sunspot.classic")

## Perform Checks
test_check("tswgewrapped")