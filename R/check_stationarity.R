#' Wrapper to check the stationarity of a Time Series 
#' @param x time series realization
#' @param title Title of the time series plot (output of function)
#' @param xlab x-label of the time series plot (output of the function)
#' @param ylab y-label of the time series plot (output of the function)
#' @export
check_stationarity = function(x, title = "Time Series Plot", xlab = "Time", ylab = "Time Series Realization" ){
  plot(x, type = "l", main = title, xlab = xlab, ylab = ylab)
  len = length(x)
  len_by_2 = round(len/2)
  seg_2_start = len_by_2+1
  acf(x[1:len], main = "Full xset")
  acf(x[1:len_by_2], main = "First Half ACF")
  acf(x[seg_2_start:len], main = "Second Half ACF")
}
