% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aic5.R
\name{aic5}
\alias{aic5}
\title{aic5: slightly faster model estimations
todo: rewrite backcast and aic.wge for real speed gains
returns the top 5 aic}
\usage{
aic5(x, p = 0:8, q = 0:5, type = "aic", silent = FALSE)
}
\arguments{
\item{p}{integer vector of the ar order}

\item{q}{integer vector of the ma order}

\item{silent}{whether or not to return output}

\item{vec}{the vector (time series object) to operate on}
}
\value{
a data frame of top 5 models
}
\description{
aic5: slightly faster model estimations
todo: rewrite backcast and aic.wge for real speed gains
returns the top 5 aic
}