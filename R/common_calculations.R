#' Computes Gamma0 (Variance of a time series realization)
#' @param x time series realization
#' @return Gamma0 (Variance of the time series realization)
#' @export
calculate_ts_gamma0 = function(x){
  n = length(x)
  gamma0 = var(x)*(n-1)/n
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
  v=var(x,na.rm = TRUE)
  gamma0=calculate_ts_gamma0(x)
  aut=acf(x,lag.max=nlag) 
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
  multiplier = qnorm(1 - alpha/2)
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
    sum = sum + phi[i] * p$aut1[i+1]
  }
  
  return (vara / (1 - sum))
}