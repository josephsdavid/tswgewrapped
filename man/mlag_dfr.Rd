% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lag.R
\name{mlag_dfr}
\alias{mlag_dfr}
\title{mlag_dfr: Lag multiple columns of a data frame}
\usage{
mlag_dfr(df, col_names, lags)
}
\arguments{
\item{df}{The dataframe containing the time series data}

\item{col_names}{A vector of column names to be used to create the lagged variables}

\item{lags}{Number of lags to create for the column}
}
\description{
mlag_dfr: Lag multiple columns of a data frame
}
\examples{
x  <-  c(2,3,5,7,9,9,5,3)
z <- c(6,9,3,3,3,4,5,7)
y <- 1:8
df <- data.frame(Y = y, X = x, Z =z)
lag_dfr(df, "X", 1:3)
mlag_dfr(df, c("X","Z"), 1:3)
}
