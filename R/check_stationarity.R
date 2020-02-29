#' Wrapper to check the stationarity of a Time Series 
#' @param data time series realization
#' @param title Title of the time series plot (output of function)
#' @param xlab x-label of the time series plot (output of the function)
#' @param ylab y-label of the time series plot (output of the function)
#' @export
check_stationarity = function(data, title = "Time Series Plot", xlab = "Time", ylab = "Time Series Realization" ){
  # Currently, this only takes a vector (data).
  # Future Improvements: Add ability to take in Data Frames directly.
  
  # Need to add these in here explicitly, else this function does not work.
  requireNamespace("ggfortify")
  requireNamespace("patchwork")
  
  # Setup
  x = data
  len = length(x)
  len_by_2 = round(len/2)
  seg_2_start = len_by_2+1
  
  # Plot Realization
  if (class(data) != "data.frame"){
    data = data.frame(Data = data, Time = seq(1:length(data)))
  }
  g1 = ggplot2::ggplot(data, ggplot2::aes_string(x = 'Time', y = 'Data')) + ggplot2::geom_line() +
    ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  # Full Data ACF
  g2 = ggplot2::autoplot(stats::acf(x, plot = FALSE), conf.int.fill = '#0000FF') + 
    ggplot2::ggtitle("Full Dataset") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  # First Half ACF
  g3 = ggplot2::autoplot(stats::acf(x[1:len_by_2], plot = FALSE), conf.int.fill = '#0000FF') + 
    ggplot2::ggtitle("First Half") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  # Second Half ACF
  g4 = ggplot2::autoplot(stats::acf(x[seg_2_start:len], plot = FALSE), conf.int.fill = '#0000FF') + 
    ggplot2::ggtitle("Second Half") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  print((g1 + g2) / (g3 + g4))  ## Requires Patchwork package
  
  return(TRUE)
  
}
