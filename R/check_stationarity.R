#' Wrapper to check the stationarity of a Time Series 
#' @param data time series data
#' @param title Title of the time series plot (output of function)
#' @param xlab x-label of the time series plot (output of the function)
#' @param ylab y-label of the time series plot (output of the function)
#' @export
check_stationarity = function(data, title = "Time Series Plot", xlab = "Time", ylab = "Time Series Realization" ){
  plot(data, type = "l", main = title, xlab = xlab, ylab = ylab)
  len = length(data)
  len_by_2 = round(len/2)
  seg_2_start = len_by_2+1
  acf(data[1:len], main = "Full Dataset")
  acf(data[1:len_by_2], main = "First Half ACF")
  acf(data[seg_2_start:len], main = "Second Half ACF")
}