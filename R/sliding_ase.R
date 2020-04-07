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
#' @param step_n.ahead Whether to step each batch by n.ahead values (Default = FALSE)
#' @param verbose How much to print during the model building and other processes (Default = 0)
#' @param ... any additional arguments to be passed to the forecast functions (e.g. max.p for sigplusnoise model, lambda for ARUMA models)
#' @return Named list 
#'         'ASEs' - ASE values
#'         'time_test_start' - Time Index indicating start of test time corresponding to the ASE values
#'         'time_test_end' - Time Index indicating end of test time corresponding to the ASE values
#'         'batch_num' - Indicates the batch number for each ASE value
#'         'f' - Forecasts for each batch
#'         'll' - Lower Forecast Limit for each batch
#'         'ul' - Upper Forecast Limit for each batch
#'         'time.forecasts' - Time Corresponding to each forecast, upper and lower limit values
#' @export
sliding_ase_univariate = function(x,
                                  phi = 0, theta = 0, d = 0, s = 0, # ARUMA arguments
                                  linear = NA, freq = NA,           # Signal + Noise arguments
                                  n.ahead = NA, batch_size = NA,    # Forecasting specific arguments
                                  step_n.ahead = TRUE,
                                  verbose = 0,
                                  ...)                              # max.p (sigplusnoise), lambda (ARUMA)      
{
  # Sliding CV ... batches are mutually exclusive
  
  n = length(x)
  
  if (is.na(batch_size)){
    if (verbose >= 1){
      cat("\nBatch Size has not been specified. Will assume a single batch.")
    }
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
  
  forecasts.f = rep(NA, n)
  forecasts.ul = rep(NA, n)
  forecasts.ll = rep(NA, n)
  time.forecasts = seq(1, n, 1)
  
  start = 1
  
  if (step_n.ahead == FALSE){
    # Step Size = 1
    step_size = 1
    num_batches = n-batch_size+1  
  }
  else{
    # Step by n.ahead each time
    step_size = n.ahead
    num_batches = floor((n-batch_size)/n.ahead)  + 1
  }
  
  if (verbose >= 1){
    cat(paste("\nNumber of batches expected: ", num_batches))
  }
  
  ASEs = numeric(num_batches)
  time_test_start = numeric(num_batches)
  time_test_end = numeric(num_batches)
  batch_num = numeric(num_batches)
  
  for (i in 0:(num_batches-1))
  {
    # Define the batch
    subset = x[start:(batch_size+i*step_size)]
    # Take last n.ahead observations from the batch and use that to compare with the forecast
    
    test_start = i*step_size + batch_size - n.ahead + 1
    test_end = i*step_size + batch_size
    data_start = start
    data_end = i*step_size + batch_size
    
    time_test_start[i+1] = test_start
    time_test_end[i+1] = test_end
    batch_num[i+1] = i+1
    
    
    test_data = x[test_start:test_end]
    
    if (aruma){
      forecasts = tswge::fore.aruma.wge(x = subset, phi = phi, theta = theta, d = d, s = s,
                                        n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
    }
    else{
      forecasts = tswge::fore.sigplusnoise.wge(x = subset, linear = linear, freq = freq,
                                               n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
    }
    
    ASEs[i+1] = mean((test_data - forecasts$f)^2)
    start = start + step_size
    
    forecasts.f[test_start: test_end] = forecasts$f 
    forecasts.ll[test_start: test_end] = forecasts$ll
    forecasts.ul[test_start: test_end] = forecasts$ul
    time.forecasts[test_start: test_end] = seq(test_start, test_end, 1)
  }
  
  return(list(ASEs = ASEs,
              time_test_start = time_test_start,
              time_test_end = time_test_end,
              batch_num = batch_num,
              f = forecasts.f,
              ll = forecasts.ll,
              ul = forecasts.ul,
              time.forecasts = time.forecasts ))
}

#' Function to calculate the sliding window ASE for a model
#' Supports VAR Model from the vars package
#' @param data The dataframe containing the time series realizations (data should not contain time index)
#' @param var_interest The output variable of interest (dependent variable)
#' @param k The lag value to use for the VAR model (generally determined by the VARselect function)
#' @param trend_type The trend type to use in VARselect and the VAR model. Refer to vars::VARselect and vars::VAR for valid options.
#' @param season The seasonality to use in the VAR model.
#' @param batch_size Window Size used 
#' @param n.ahead last n.ahead data points in each batch will be used for prediction and ASE calculations
#' @param step_n.ahead Whether to step each batch by n.ahead values (Default = FALSE)
#' @param verbose How much to print during the model building and other processes (Default = 0)
#' @param ... Additional arguments to pass to the VAR model
#' @return Named list 
#'         'ASEs' - ASE values
#'         'time_test_start' - Time Index indicating start of test time corresponding to the ASE values
#'         'time_test_end' - Time Index indicating end of test time corresponding to the ASE values
#'         'batch_num' - Indicates the batch number for each ASE value
#'         'AICs' = The AIC values for the individual batches
#'         'BICs' = The BIC values for the individual batches
#'         'f' - Forecasts for each batch
#'         'll' - Lower Forecast Limit for each batch
#'         'ul' - Upper Forecast Limit for each batch
#'         'time.forecasts' - Time Corresponding to each forecast, upper and lower limit values
#' @export
sliding_ase_var = function(data, var_interest,
                           k, trend_type = NA, season = NULL,
                           n.ahead = NA, batch_size = NA,    # Forecasting specific arguments
                           step_n.ahead = TRUE, verbose = 0,
                           ...
                           )                              
{
  # Sliding CV ... batches are mutually exclusive
  
  
  n = nrow(data)
  
  if (is.na(batch_size)){
    message("Batch Size has not been specified. Will assume a single batch")
    batch_size = n
  }
  
  if (is.na(n.ahead)){
    stop("Number of points to be used for forecasting has not been specified. Please specify n.ahead")
  }
  
  if (all(k == 0) & all(is.na(trend_type))){
    stop("You have specified the arguments for building the VAR model. Please specify at least one of these to continue")
  }
  
  forecasts.f = rep(NA, n)
  forecasts.ul = rep(NA, n)
  forecasts.ll = rep(NA, n)
  time.forecasts = seq(1, n, 1)
  
  start = 1
  
  if (step_n.ahead == FALSE){
    # Step Size = 1
    step_size = 1
    num_batches = n-batch_size+1  
  }
  else{
    # Step by n.ahead each time
    step_size = n.ahead
    num_batches = floor((n-batch_size)/n.ahead)  + 1
  }
  
  if (verbose >= 1){
    cat(paste("\nNumber of batches expected: ", num_batches))
  }
  
  ASEs = numeric(num_batches)
  time_test_start = numeric(num_batches)
  time_test_end = numeric(num_batches)
  batch_num = numeric(num_batches)
  
  for (i in 0:(num_batches-1))
  {
    # Define the batch
    batch_start = start
    batch_end = i*step_size + batch_size
    
    batch = data[batch_start:batch_end, ]
    # Take last n.ahead observations from the batch and use that to compare with the forecast
    
    train_start = start
    train_end = nrow(batch)-n.ahead
    
    train_data = batch[1:(nrow(batch)-n.ahead),]
    
    test_start = i*step_size + batch_size - n.ahead + 1
    test_end = i*step_size + batch_size
    
    time_test_start[i+1] = test_start
    time_test_end[i+1] = test_end
    batch_num[i+1] = i+1
    
    test_data = data[test_start:test_end, ]
    
    # Fit model for the batch
    varfit = vars::VAR(train_data, p=k, type=trend_type, season = season, ...)
    
    if (verbose >= 2){
      cat(paste0("\nBatch: ", i+1))
      cat(paste0("\nPrinting summary of the VAR fit for the variable of interest: ", var_interest, "\n\n"))
      print(summary(varfit$varresult[[var_interest]]))
    }
   
    
    # Forecast for the batch
    forecasts = stats::predict(varfit, n.ahead=n.ahead)
    forecasts = forecasts[['fcst']][[var_interest]] ## Get the forecasts only for the dependent variable
    
    ASEs[i+1] = mean((test_data[, var_interest] - forecasts[, 'fcst'])^2)
    start = start + step_size
    
    forecasts.f[test_start: test_end] = forecasts[, 'fcst']  
    forecasts.ll[test_start: test_end] = forecasts[, 'lower']
    forecasts.ul[test_start: test_end] = forecasts[, 'upper']
    time.forecasts[test_start: test_end] = seq(test_start, test_end, 1)
    
  }
  
  return(list(ASEs = ASEs,
              time_test_start = time_test_start,
              time_test_end = time_test_end,
              batch_num = batch_num,
              f = forecasts.f,
              ll = forecasts.ll,
              ul = forecasts.ul,
              time.forecasts = time.forecasts ))
}

