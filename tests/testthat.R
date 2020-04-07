## Load Libraries
library(testthat)
library(tswge)
library(tswgewrapped)

## This is absolutely needed, else we get errors that "there is no package called 'lattice'" in the unit tests
if(!require("lattice")){
  # https://github.com/topepo/caret/issues/411#issuecomment-209973908
  install.packages("lattice", repos = "http://cran.us.r-project.org", dependencies = c("Depends", "Imports", "Suggests"))
}

if(!require("forecast")){
  # https://github.com/topepo/caret/issues/411#issuecomment-209973908
  install.packages("forecast", repos = "http://cran.us.r-project.org", dependencies = c("Depends", "Imports", "Suggests"))
}

file = system.file("extdata", "USeconomic.csv", package = "tswgewrapped", mustWork = TRUE)
USeconomic = read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
names(USeconomic) = gsub("[(|)]", "", colnames(USeconomic))

## Load Datasets
data("airlog")
data("AirPassengers")
data("sunspot.classic")

## Perform Checks
test_check("tswgewrapped")