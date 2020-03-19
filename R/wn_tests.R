#' ljung box test for white noise
#' @param x the time series
#' @param p the ar order (Default = 0)
#' @param q the ma order (Default = 0)
#' @param k_val a vector of k values
#' @param model_name Model name or identifier (Default: "My Model")
#' @param alpha Significance level to be used for ljung_box tests
#' @return the results of the tests, in tidy data format
#' @examples 
#' library(tswge)
#' 
#' # Generated White Noise 
#' wn = gen.arma.wge(n = 200, sn = 101)
#' ljung_box(wn)
#' 
#' # Not White Noise
#' data(hadley) 
#' ljung_box(hadley)
#'
#' @export
ljung_box <- function(x, p = 0, q = 0, k_val = c(24,48), model_name = "My Model", alpha = 0.05) {

  ljung <- function(k) {
    hush(tswge::ljung.wge(x = x, p = p, q = q, K = k))
  }
  tests = sapply(k_val, ljung)
  
  rvData = data.frame(t(tests)) %>% 
    purrr::map(unlist) %>% 
    dplyr::as_tibble() %>% 
    tibble::remove_rownames() %>%
    dplyr::mutate(Model = model_name, 
                  Decision = ifelse(.data$pval < alpha, "REJECT NULL", "FTR NULL"))

  if (any(rvData$Decision == 'REJECT NULL')){
    cat(paste("At least one of the 'ljung_box' tests rejected the null hypothesis that the data is consistent with white noise at an significance level of ", alpha, " \n"))
  }
  else{
    cat(paste("None of the 'ljung_box' tests rejected the null hypothesis that the data is consistent with white noise at an significance level of ", alpha, " \n"))
  }
  
  return(rvData)
  
}

#' Evaluate if the dats is consistent with white noise
#' @param x the time series
#' @param p the ar order (Default = 0)
#' @param q the ma order (Default = 0)
#' @param k_val a vector of k values for ljung_box test
#' @param alpha Significance level to be used for ljung_box tests
#' @param lag.max Value of lags to plot for the ACF (Default: 50)
#' @param model_name Model name or identifier (Default: "My Model")
#' @return the results of the tests, in tidy data format
#' @examples
#' library(tswge)
#' 
#' # Generated White Noise 
#' wn = gen.arma.wge(n = 200, sn = 101)
#' white_noise_eval(wn)
#' 
#' # Not White Noise
#' data(hadley) 
#' white_noise_eval(hadley)
#' 
#' @export
white_noise_eval = function(x, p = 0, q = 0, k_val = c(24,48), alpha = 0.05, lag.max = 50, model_name = "My Model"){
  # Need to add these in here explicitly, else this function does not work.
  requireNamespace("ggfortify")
  requireNamespace("patchwork")
  
  # Setup
  len = length(x)
  
  # Plot Realization
  if (class(x) != "data.frame"){
    data = data.frame(Data = x, Time = seq_along(x))
  }
  
  g1 = ggplot2::ggplot(data, ggplot2::aes_string(x = 'Time', y = 'Data')) + ggplot2::geom_line() +
    ggplot2::ggtitle("Step 1A: Visual Realization Check") + ggplot2::xlab("Time") + ggplot2::ylab("Realization") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  # Full Data ACF
  g2 = ggplot2::autoplot(stats::acf(x, plot = FALSE, lag.max = lag.max), conf.int.fill = '#0000FF') + 
    ggplot2::ggtitle("Step 1B: Visual ACF Check") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  print(g1 + g2)  ## Requires Patchwork package
  
  # Return output of ljung_box tests
  ljung_box(x, p = p, q = q, k_val = k_val, alpha = alpha, model_name = model_name)
}
