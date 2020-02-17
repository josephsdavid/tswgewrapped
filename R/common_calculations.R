#' Computes Gamma0 (Variance of a time series realization)
#' @param x time series realization
#' @return Gamma0 (Variance of the time series realization)
#' @export
calculate_ts_gamma0 = function(x){
  n = length(x)
  gamma0 = stats::var(x)*(n-1)/n
  return(gamma0)
}

#' Computes the Mean of a Time Series realization
#' @param x time series realization
#' @return Mean of the Time Series realization
#' @export
calculate_ts_mean = function(x){
  return(mean(x))
}

#' Computes the Variance of the Sampling distribution of the Mean of a Time Series
#' @param x time series realization
#' @return Variance of the Sampling distribution of the Mean of the Time Series
#' @export
calculate_ts_var_of_mean = function(x){
  n=length(x) 
  nlag=n-1 
  m=mean(x)
  v=stats::var(x,na.rm = TRUE)
  gamma0=calculate_ts_gamma0(x)
  aut = stats::acf(x,lag.max=nlag) 
  sum=0
  for (k in 1:nlag) {
    sum=sum+(1-k/n)*aut$acf[k+1]*gamma0
  }
  
  vxbar=2*sum/n + gamma0/n 
  return(vxbar)  
}

#' Computes the Confidence Interval of the Mean of a Time Series
#' @param x time series realization
#' @return Confidence Interval of the Mean of a Time Series
#' @export
calculate_ts_mean_confidence_interval = function(x, alpha = 0.05){
  xbar = calculate_ts_mean(x)
  vxbar = calculate_ts_var_of_mean(x)
  multiplier = stats::qnorm(1 - alpha/2)
  ci = c(xbar - multiplier * sqrt(vxbar), xbar +  multiplier * sqrt(vxbar))
  return(ci)
}

#' Computes the Population Variance of an AR(1) Time Series
#' @param phi phi values of the time series (only 1 in this case)
#' @param vara variance of the noise term
#' @return True Population Variance of the Time Series
#' @export
calculate_ar1_varx = function(phi, vara=1){
  # Computes SigmaX^2 = vara/(1-phi^2)
  # actually, we can use calculate_arp_varx for this directly since rho1 = phi1^k where k = 1
  return (vara/(1 - phi^2))
} 

#' Computes the Population Variance of an AR(p) Time Series
#' Note that this can also be used for AR(1) models instead of using calculate_ar1_varx
#' although, it will need an extra argument 'pt'
#' @param phi phi values of the time series
#' @param pt output of plotts.true.wge
#' @param vara variance of the noise term
#' @return True Population Variance of the Time Series
#' @export
calculate_arp_varx = function(phi, pt, vara = 1){
  # Computes SigmaX^2 = vara/(1-phi1*rho1 - phi2*rho2 - ...)
  sum = 0
  for (i in 1:length(phi)){
    sum = sum + phi[i] * pt$aut1[i+1]
  }
  
  return (vara / (1 - sum))
}

#' Given an AR(p,q) realization, this function estimates the white 
#' noise estimates using equation 6.23 (from the text book)
#' @param x time series realization
#' @param phi phi values of the time series
#' @param theta theta values of the time series
#' @param index time value till which the white noise estimates are needed
#' @return All white noise estimates till the index
#' @export
compute_a = function(x, phi, theta, index){
  
  ## Limit 1
  get_a_i_lesseq_p = function(index){
    all_a = rep(0,index)  # Return all 0s till the index
    return(all_a)
  }
  
  ## Limit 2
  get_a_i_lesseq_lenX = function(x, p, q, phi, theta, index){
    all_a = get_a_i_lesseq_p(p)  ## get all 0's till p
    
    ## Then compute the values one by one till needed
    limit = min(index, length(x))
    for (i in (p+1):limit){
      a = x[i] - sum(phi * x[(i-1):(i-p)]) + sum(theta * all_a[(i-1):(i-q)]) - (1 - sum(phi)) * mean(x)
      all_a = c(all_a, a)
    }
    
    return(all_a)
  }
  
  ## Limit 3
  get_a_i_more_lenX = function(x, p, q, phi, theta, index){
    n = length(x)
    all_a = get_a_i_lesseq_lenX(x, p, q, phi, theta, n)  ## Get all values till len(x)
    all_a = c(all_a, rep(0, (index-n)))  ## Then append 0s after that
    return(all_a)
  }
  
  
  p = length(phi)
  q = length(theta)
  
  all_a = c()
  
  if (index <= p){
    all_a = get_a_i_lesseq_p(index)
  }
  else if (index <= length(x)){
    all_a = get_a_i_lesseq_lenX(x, p, q, phi, theta, index)
  }
  else{
    all_a = get_a_i_more_lenX(x, p, q, phi, theta, index)
  }
  
  return(all_a)
}

#' Given the white noise estimates, this function computes 
#' the variance of the white noise. Note that non-zero terms 
#' are removed before calcualting the variance.
#' @param all_a White Noise Estimates
#' @return Variance of the White Noise terms
#' @export
compute_vara = function(all_a){
  subset = all_a[all_a != 0]
  len_subset = length(subset)
  vara = sum(subset^2)/length(subset)
  return(vara)
}

#' Given the white noise estimates, this function computes 
#' the standard deviation of the white noise. Note that non-zero 
#' terms are removed before calcualting the variance.
#' @param all_a White Noise Estimates
#' @return Standard Deviation of the White Noise terms
#' @export
compute_stda = function(all_a){
  return(sqrt(compute_vara(all_a)))
}

#' Given an AR(p,q) realization, this function estimates the white 
#' noise estimates using equation 6.23 (from the text book), the 
#' variance and the standard deviation of the white noise
#' @param x time series realization
#' @param phi phi values of the time series
#' @param theta theta values of the time series
#' @param index time value till which the white noise estimates are needed
#' @return All white noise estimates, variance and standard deviation of these estimates
#' @export
return_all_a_calc = function(x, phi, theta){
  all_a = compute_a(x = x, phi = phi, theta = theta, index = length(x))
  vara = compute_vara(all_a)
  stda = compute_stda(all_a)
  return(list(all_a = all_a, vara = vara, stda = stda))
}