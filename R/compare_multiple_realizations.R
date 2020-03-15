#' Generate Multiple Realizations of a model
#' Useful for checking model appropriateness 
#' @param x The actual time series realization
#' @param phi 'phi' values for the AR portion of the model (Default = 0)
#' @param theta 'theta' values for the AR portion of the model (Default = 0)
#' @param d 'd' value for the ARIMA portion of the model (Default = 0)
#' @param s seasonality 's' of the ARIMA portion of the model (Default = 0)
#' @param vara The white noise variance of the model
#' @param n.realizations The number of hypothetical realizations
#'                       to create for the model (Default = 4)
#' @param lag.max The maximum amount of lag data to capture (Default = 25)
#' @param seed The seed to use for reproducibility
#' @param model_name The name to be assigned to the model (Default = "Custom Model")
#' @param return What values to return (Default = 'all')
#'              'all': Returns the actual along with the hypothetical realization data   
#'              'actual': Returns only the actual realization data   
#'              'hypothetical': Returns only the hypothetical realization data
#' @return Named list with 'data' and 'results'
#'         'data' contains the actual and the hypothetical realizations
#'                in tidy data format (appropriate for plotting)
#'         'results' contain the ACF and Spectral Density values for the
#'                   actual and hypothetical realizations in tidy data format 
#' @examples 
#' library(tswge)
#' data("sunspot.classic")
#' est = est.arma.wge(sunspot.classic,p=8)
#' 
#' r = generate_multiple_realization(x = sunspot.classic,
#'                                   phi = est$phi, theta = est$theta, vara = est$avar,
#'                                   seed = 11)
#' @export
#' 
generate_multiple_realization = function(x, phi = 0, theta = 0, d = 0, s = 0, vara = NA,
                                         n.realizations = 4, lag.max = 25, seed = NA,
                                         model_name = "Custom Model", return = 'all'){
  
  library(magrittr)
  
  if (all(is.na(vara))){
    stop("You have not specified the white noise variance estimate for your model. Please specify to proceed.")
  }
  
  if (!(return %in% c("all", "actual", "hypothetical"))){
    stop("The 'return' argument has not been provided with the correct values. Acceptable values are 'all', 'actual' or 'hypothetical'")
  }
  
  if((return == 'all') | (return == 'hypothetical')){
    if (!all(is.na(seed))){
      set.seed(seed)
    }
    
    realizations = list()
    for (i in 1:n.realizations){
      realizations[[i]] = tswge::gen.aruma.wge(n = length(x), phi = phi, theta = theta, d = d, s = s,
                                               vara = vara,
                                               sn = sample(1:10000, 1, replace = FALSE),
                                               plot = FALSE) + mean(x)
    }
  }
  
  results = dplyr::tribble(~Model, ~Realization, ~Characteristic, ~Value, ~Index) 
  
  if((return == 'all') | (return == 'actual')){
    # Add the actual realization ACF
    aut = stats::acf(x, lag.max = lag.max, plot = FALSE)
    results = results %>% dplyr::add_row(Model = model_name,
                                         Realization = "Actual",
                                         Characteristic = "ACF",
                                         Value = aut$acf[ , , 1],
                                         Index = 0:lag.max)
  }
  
  if((return == 'all') | (return == 'hypothetical')){  
    for (i in 1:n.realizations){
      aut = stats::acf(realizations[[i]], lag.max = lag.max, plot = FALSE)
      results = results %>% dplyr::add_row(Model = model_name,
                                           Realization = i,
                                           Characteristic = "ACF",
                                           Value = aut$acf[ , , 1],
                                           Index = 0:lag.max)
    }
  }
  
  if((return == 'all') | (return == 'actual')){
    # Add the actual realization Spectral Density
    spectrum_all = tswge::parzen.wge(x, plot = FALSE)
    spectrum  = spectrum_all$pzgram
    freq = spectrum_all$freq
    results = results %>% dplyr::add_row(Model = model_name,
                                         Realization = "Actual",
                                         Characteristic = "Spectrum",
                                         Value = spectrum,
                                         Index = freq)
  }
  
  if((return == 'all') | (return == 'hypothetical')){
    for (i in 1:n.realizations){
      spectrum_all = tswge::parzen.wge(realizations[[i]], plot = FALSE)
      spectrum  = spectrum_all$pzgram
      freq = spectrum_all$freq
      results = results %>% dplyr::add_row(Model = model_name,
                                           Realization = i,
                                           Characteristic = "Spectrum",
                                           Value = spectrum,
                                           Index = freq)
    }
  }
  
  results = results %>% 
    dplyr::mutate(Realization = as.factor(.$Realization))
  
  data = dplyr::tribble(~Model, ~Realization, ~Data, ~Index)
  
  if((return == 'all') | (return == 'actual')){
    data = data %>% dplyr::add_row(Model = model_name,
                                   Realization = "Actual",
                                   Data = x,
                                   Index = 1:length(x))
  }
  
  if((return == 'all') | (return == 'hypothetical')){
    for (i in 1:n.realizations){
      data = data %>% dplyr::add_row(Model = model_name,
                                     Realization = i,
                                     Data = realizations[[i]],
                                     Index = 1:length(realizations[[i]]))
    }
  }
  
  return(list(data = data, results = results))
}

#' Plot multiple realizations of a model
#' Useful for checking model appropriateness 
#' @param data The actual and the hypothetical realizations in tidy data format
#' @param results The ACF and Spectral Density values for the 
#'                actual and hypothetical realizations in tidy data format 
#' @param scales The scales argument to be passed to ggplot facet_wrap layer
#'               (Default = 'free_y') Other appropriate options: 'fixed'
#' @examples 
#' library(tswge)
#' data("sunspot.classic")
#' est = est.arma.wge(sunspot.classic,p=8)
#' 
#' r = generate_multiple_realization(x = sunspot.classic,
#'                                   phi = est$phi, theta = est$theta, vara = est$avar,
#'                                   seed = 11)
#' plot_multiple_realizations(data = r$data, results = r$results)
#' @export
#'
plot_multiple_realizations = function(data, results, scales = 'free_y'){
  
  library(magrittr)
  requireNamespace("ggfortify")
  requireNamespace("patchwork")
  
  n.realizations = length(unique(results$Realization)) - 1
  
  for (name in unique(data$Model)){
    data_hypothetical = data %>% 
      dplyr::filter(.$Model == name) %>% 
      dplyr::filter(.$Realization != 'Actual')
    
    data_actual = data %>% 
      dplyr::filter(.$Model == name) %>% 
      dplyr::filter(.$Realization == 'Actual')
    
    g1 = ggplot2::ggplot() +   
      ggplot2::geom_line(data = data_actual, mapping = ggplot2::aes_string(x = 'Index', y = 'Data', color = 'Realization'), size = 2) +
      ggplot2::scale_color_manual(values=c('blue')) +
      ggplot2::xlab("Time") + ggplot2::ylab("Actual Realization") + 
      ggplot2::theme(legend.position="none")
    
    g2 = ggplot2::ggplot() +   
      ggplot2::geom_line(data = data_hypothetical, mapping = ggplot2::aes_string(x = 'Index', y = 'Data', color = 'Realization')) +
      ggplot2::facet_wrap( ~ Model + Realization, ncol=2, scales = scales) +
      ggplot2::scale_color_manual(values=c(rep("black", n.realizations))) +
      ggplot2::ggtitle("Realization Comparison") + ggplot2::xlab("Time") + ggplot2::ylab("Hypothetical Realizations") +
      ggplot2::theme(legend.position="none")
    
    print(g1 / g2)
  }
  
  
  
  
  acf_data_hypothetical = results %>% 
    dplyr::filter(.$Realization != 'Actual') %>% 
    dplyr::filter(.$Characteristic == 'ACF')
  
  acf_data_actual = results %>% 
    dplyr::filter(.$Realization == 'Actual') %>% 
    dplyr::filter(.$Characteristic == 'ACF')
  
  g3 = ggplot2::ggplot() + 
    ggplot2::facet_wrap(~Model, ncol = 2, scales = scales) +
    ggplot2::geom_line(data = acf_data_actual, mapping = ggplot2::aes_string(x = 'Index', y = 'Value', color = 'Realization'), size = 2) +
    ggplot2::geom_line(data = acf_data_hypothetical, mapping = ggplot2::aes_string(x = 'Index', y = 'Value', color = 'Realization')) +
    ggplot2::scale_color_manual(values=c(rep("black", n.realizations), 'blue')) +
    ggplot2::ggtitle("ACF Comparison") + ggplot2::xlab("Lag") + ggplot2::ylab("ACF")
  
  print(g3)
  
  
  spectrum_data_hypothetical = results %>% 
    dplyr::filter(.$Realization != 'Actual') %>% 
    dplyr::filter(.$Characteristic == 'Spectrum')
  
  spectrum_data_actual = results %>% 
    dplyr::filter(.$Realization == 'Actual') %>% 
    dplyr::filter(.$Characteristic == 'Spectrum')
  
  g4 = ggplot2::ggplot() +   
    ggplot2::facet_wrap(~Model, ncol = 2, scales = scales) +
    ggplot2::geom_line(data = spectrum_data_actual, mapping = ggplot2::aes_string(x = 'Index', y = 'Value', color = 'Realization'), size = 2) +
    ggplot2::geom_line(data = spectrum_data_hypothetical, mapping = ggplot2::aes_string(x = 'Index', y = 'Value', color = 'Realization')) +
    ggplot2::scale_color_manual(values=c(rep("black", n.realizations), 'blue')) +
    ggplot2::ggtitle("Spectral Density Comparison") + ggplot2::xlab("Frequency") + ggplot2::ylab("Spectral Density (dB)")
  
  print(g4)
  
  
  
}











