#' Function to calculate the sliding window ASE for a model
#' Supports ARMA, ARIMA, ARUMA (seasonal ARIMA) and Signal Plus Noise Models 
#' @param x time series realization
#' @param phi phi values associated with the ARIMA model
#' @param theta theta values associated with the ARIMA model
#' @param d differencing 'd' associated with the ARIMA model
#' @param s seasonality 's' of the ARIMA model
#' @param linear (TRUE|FALSE) If using a Signal Plus Noise model, should a linear signal be used?
#' @param freq If using a sinusoidal signal, what is the frequency of the signal?
#' @param batch_size Window Size used 
#' @param n.ahead last n.ahead data points in each batch will be used for prediction and ASE calculations
#' @param time_index What time index to return for each ASE. Options are data_start, data_end, test_start and test_end (Defaults to test_start)
#' @param ... any additional arguments to be passed to the forecast functions (e.g. max.p for sigplusnoise model, lambda for ARUMA models)
#' @return Named list ('ASEs' - ASE values and 'Time' - Corresponding Time stamps based on time_index argument) 
#' @export
sliding_ase = function(x,
                       phi = 0, theta = 0, d = 0, s = 0, # ARUMA arguments
                       linear = NA, freq = NA,           # Signal + Noise arguments
                       n.ahead = NA, batch_size = NA,    # Forecasting specific arguments
                       time_index = 'test_start',        # Time Index to return ("<test|data>_<start|end>") 
                       ...)                              # max.p (sigplusnoise), lambda (ARUMA)      
{
  # Sliding CV ... batches are mutually exclusive
  
  n = length(x)
  
  if (is.na(batch_size)){
    warning("Batch Size has not been specified. Will assume a single batch")
    cat("\n")
    batch_size = n
  }
  
  if (is.na(n.ahead)){
    stop("Number of points to be used for forecasting has not been specified. Please specify n.ahead")
  }
  
  if (all(phi == 0) & all(theta == 0) & d == 0 & s == 0){
    if (is.na(linear) & is.na(freq)){
      stop("You have specified the arguments for neither an ARMA/ARUMA model or a Signal + Noise Model. Please specify at least one of these to continue")
    }
  }
  
  aruma = FALSE
  if (!(all(phi == 0) & all(theta == 0) & d == 0 & s == 0)){
    aruma = TRUE
  }
  else{
    # Signal + Noise model
  }
  
  start = 1
  num_batches = n-batch_size+1
  ASEs = numeric(num_batches)
  time = numeric(num_batches)
  
  for (i in 0:(num_batches-1))
  {
    # Define the batch
    subset = x[start:(batch_size+i)]
    # Take last n.ahead observations from the batch and use that to compare with the forecast
    
    if (time_index == 'test_start'){
      time[i+1] = batch_size+i-n.ahead+1
    }
    else if (time_index == 'test_end'){
      time[i+1] = batch_size+i
    }
    else if (time_index == 'data_start'){
      time[i+1] = start
    }
    else if (time_index == 'data_end'){
      time[i+1] = batch_size+i-n.ahead
    }
    else{
      stop("You have not provided the correct time_index. Options are <data|test>_<start_end>")
    }
    
    test_data = x[(batch_size+i-n.ahead+1):(batch_size+i)]
    
    if (aruma){
      forecasts = tswge::fore.aruma.wge(x = subset, phi = phi, theta = theta, d = d, s = s,
                                        n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
    }
    else{
      forecasts = tswge::fore.sigplusnoise.wge(x = subset, linear = linear, freq = freq,
                                               n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
    }
    
    ASEs[i+1] = mean((test_data - forecasts$f)^2)
    start = start+1
  }
  
  return(list(ASEs = ASEs, time = time))
}