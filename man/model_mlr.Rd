% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvar_mlr.R
\name{model_mlr}
\alias{model_mlr}
\title{Model as MLR model}
\usage{
model_mlr(df, formula)
}
\arguments{
\item{df}{The dataframe containing the time series data}

\item{formula}{Formula to be used for model creation}
}
\description{
Model as MLR model
}
\examples{
model_mlr(mtcars, mpg~.)
model_mlr(mtcars, mpg~cyl + hp)
}
