% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sliding_ase.R
\name{sliding_ase_univariate}
\alias{sliding_ase_univariate}
\title{Function to calculate the sliding window ASE for a model
Supports ARMA, ARIMA, ARUMA (seasonal ARIMA) and Signal Plus Noise Models}
\usage{
sliding_ase_univariate(
  x,
  phi = 0,
  theta = 0,
  d = 0,
  s = 0,
  linear = NA,
  freq = NA,
  n.ahead = NA,
  batch_size = NA,
  step_n.ahead = TRUE,
  verbose = 0,
  ...
)
}
\arguments{
\item{x}{time series realization}

\item{phi}{phi values associated with the ARIMA model}

\item{theta}{theta values associated with the ARIMA model}

\item{d}{differencing 'd' associated with the ARIMA model}

\item{s}{seasonality 's' of the ARIMA model}

\item{linear}{(TRUE|FALSE) If using a Signal Plus Noise model, should a linear signal be used?}

\item{freq}{If using a sinusoidal signal, what is the frequency of the signal?}

\item{n.ahead}{last n.ahead data points in each batch will be used for prediction and ASE calculations}

\item{batch_size}{Window Size used}

\item{step_n.ahead}{Whether to step each batch by n.ahead values (Default = FALSE)}

\item{verbose}{How much to print during the model building and other processes (Default = 0)}

\item{...}{any additional arguments to be passed to the forecast functions (e.g. max.p for sigplusnoise model, lambda for ARUMA models)}
}
\value{
Named list 
        'ASEs' - ASE values
        'time_test_start' - Time Index indicating start of test time corresponding to the ASE values
        'time_test_end' - Time Index indicating end of test time corresponding to the ASE values
        'batch_num' - Indicates the batch number for each ASE value
        'f' - Forecasts for each batch
        'll' - Lower Forecast Limit for each batch
        'ul' - Upper Forecast Limit for each batch
        'time.forecasts' - Time Corresponding to each forecast, upper and lower limit values
}
\description{
Function to calculate the sliding window ASE for a model
Supports ARMA, ARIMA, ARUMA (seasonal ARIMA) and Signal Plus Noise Models
}
