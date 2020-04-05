
# TODO: Add checks for model that have seasonality
# TODO: Add checks for adding and removing models after object initialization


#### Batch ASE = FALSE ####
test_that("Batch ASE = FALSE", {

  # data("USeconomic")
  data = as.data.frame(USeconomic)
  colnames(data) = c("logM1", "logGNP", "rs", "rl")
  
  lag.max = 10
  
  models = list("AIC None" = list(select = "aic", trend_type = "none", lag.max = lag.max),
                "AIC Trend"    = list(select = "aic", trend_type = "trend", lag.max = lag.max),
                "AIC Both" = list(select = "aic", trend_type = "both", lag.max = lag.max),
                "BIC None" = list(select = "bic", trend_type = "none", lag.max = lag.max),
                "BIC Trend"    = list(select = "bic", trend_type = "trend", lag.max = lag.max),
                "BIC Both" = list(select = "bic", trend_type = "both", lag.max = lag.max)
  )
  var_interest = 'logGNP'
  
  mdl_build = ModelBuildMultivariateVAR$new(data = data, var_interest = var_interest,
                                            mdl_list = models, verbose = 0)
  
  
  recommendations = mdl_build$get_recommendations()
  mdl_build$build_recommended_models()
  
  # Get only user defined models
  models = mdl_build$get_final_models(subset = 'u')
  
  
  #### With sliding ASE = FALSE
  
  for (name in names(models)){
    models[[name]][['sliding_ase']] = FALSE
  }
  
  n.ahead = 2
  
  #### With n_step.ahead = TRUE (Default)              
  mdl_compare = ModelCompareMultivariateVAR$new(data = data, var_interest = var_interest,
                                             mdl_list = models, n.ahead = n.ahead)
  
  
  mdl_compare$get_xIC() 
  mdl_compare$plot_simple_forecasts()
  mdl_compare$plot_batch_forecasts(only_sliding = FALSE)
  mdl_compare$plot_batch_ases(only_sliding = FALSE)
  mdl_compare$plot_boxplot_ases()
  mdl_compare$statistical_compare()
  
  ## Compute ASE values from object
  ASEs = mdl_compare$get_tabular_metrics(ases = TRUE)
  
  ASE_aic_none_a  = ASEs %>% dplyr::filter(Model == "AIC None") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_aic_trend_a = ASEs %>% dplyr::filter(Model == "AIC Trend") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_aic_both_a  = ASEs %>% dplyr::filter(Model == "AIC Both") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_bic_none_a  = ASEs %>% dplyr::filter(Model == "BIC None") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_bic_trend_a = ASEs %>% dplyr::filter(Model == "BIC Trend") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_bic_both_a  = ASEs %>% dplyr::filter(Model == "BIC Both") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  
  ## Compute Forecasts, Upper Limits and Lower Limits from object
  forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
  
  summary = forecasts %>% 
    dplyr::group_by(Model) %>% 
    dplyr::summarise(MeanForecast = mean(f, na.rm = TRUE),
                     MeanLL = mean(ll, na.rm = TRUE),
                     MeanUL = mean(ul, na.rm = TRUE)
    )
  
  meanForecast_aic_none_a = summary %>% dplyr::filter(Model == "AIC None") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_aic_none_a = summary %>% dplyr::filter(Model == "AIC None") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_aic_none_a = summary %>% dplyr::filter(Model == "AIC None") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_aic_trend_a = summary %>% dplyr::filter(Model == "AIC Trend") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_aic_trend_a = summary %>% dplyr::filter(Model == "AIC Trend") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_aic_trend_a = summary %>% dplyr::filter(Model == "AIC Trend") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_aic_both_a = summary %>% dplyr::filter(Model == "AIC Both") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_aic_both_a = summary %>% dplyr::filter(Model == "AIC Both") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_aic_both_a = summary %>% dplyr::filter(Model == "AIC Both") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_bic_none_a = summary %>% dplyr::filter(Model == "BIC None") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_bic_none_a = summary %>% dplyr::filter(Model == "BIC None") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_bic_none_a = summary %>% dplyr::filter(Model == "BIC None") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_bic_trend_a = summary %>% dplyr::filter(Model == "BIC Trend") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_bic_trend_a = summary %>% dplyr::filter(Model == "BIC Trend") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_bic_trend_a = summary %>% dplyr::filter(Model == "BIC Trend") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_bic_both_a = summary %>% dplyr::filter(Model == "BIC Both") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_bic_both_a = summary %>% dplyr::filter(Model == "BIC Both") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_bic_both_a = summary %>% dplyr::filter(Model == "BIC Both") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  
  
  ## Compute values computed manually to how the object calculates it
  n = nrow(data)
  train_data = data %>% dplyr::filter(dplyr::row_number() <= (n - n.ahead))
  test_data = data %>%  dplyr::filter(dplyr::row_number() > (n - n.ahead))

  trend_types = rep(c("none", "trend", "both"),2)
  ks = c(3,3,10,2,2,2)
  
  ASEs = c()
  mean_forecasts = c()
  mean_lls = c()
  mean_uls = c()
  
  for (i in seq_along(trend_types)){

    trend_type = trend_types[i]
    k = ks[i]
    
    ## VAR Model
    varfit = vars::VAR(train_data, p=k, type=trend_type)
    
    ## Predictions
    preds = stats::predict(varfit, n.ahead=n.ahead)
    
    results = preds$fcst[[var_interest]] %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(Time = seq(n-n.ahead+1,n,1)) 
    
    ## ASE
    data_all = data %>% 
      dplyr::mutate(Time = dplyr::row_number())
    
    ASE_data = data_all %>% 
      dplyr::full_join(results, by = "Time") %>% 
      na.omit()
    
    ASE = mean((ASE_data[[var_interest]] - ASE_data$fcst)^2, na.rm = TRUE)
    ASEs = c(ASEs, ASE)
    
    mean_forecast = mean(ASE_data$fcst, na.rm = TRUE)
    mean_ll = mean(ASE_data$lower, na.rm = TRUE)
    mean_ul = mean(ASE_data$upper, na.rm = TRUE)
      
    mean_forecasts = c(mean_forecasts, mean_forecast)
    mean_lls = c(mean_lls, mean_ll)
    mean_uls = c(mean_uls, mean_ul)
    
  }
  
  ## Compare ASE Values
  expect_equal(ASE_aic_none_a, ASEs[1])
  expect_equal(ASE_aic_trend_a, ASEs[2])
  expect_equal(ASE_aic_both_a, ASEs[3])
  expect_equal(ASE_bic_none_a, ASEs[4])
  expect_equal(ASE_bic_trend_a, ASEs[5])
  expect_equal(ASE_bic_both_a, ASEs[6])

  ## Compare Forecast, Upper Limit and Lower Limits
  expect_equal(meanForecast_aic_none_a, mean_forecasts[1])
  expect_equal(meanForecast_aic_trend_a, mean_forecasts[2])
  expect_equal(meanForecast_aic_both_a, mean_forecasts[3])
  expect_equal(meanForecast_bic_none_a, mean_forecasts[4])
  expect_equal(meanForecast_bic_trend_a, mean_forecasts[5])
  expect_equal(meanForecast_bic_both_a, mean_forecasts[6])

  ## Compare Lower Limits
  expect_equal(meanLL_aic_none_a, mean_lls[1])
  expect_equal(meanLL_aic_trend_a, mean_lls[2])
  expect_equal(meanLL_aic_both_a, mean_lls[3])
  expect_equal(meanLL_bic_none_a, mean_lls[4])
  expect_equal(meanLL_bic_trend_a, mean_lls[5])
  expect_equal(meanLL_bic_both_a, mean_lls[6])

  ## Compare Upper Limits
  expect_equal(meanUL_aic_none_a, mean_uls[1])
  expect_equal(meanUL_aic_trend_a, mean_uls[2])
  expect_equal(meanUL_aic_both_a, mean_uls[3])
  expect_equal(meanUL_bic_none_a, mean_uls[4])
  expect_equal(meanUL_bic_trend_a, mean_uls[5])
  expect_equal(meanUL_bic_both_a, mean_uls[6])
  
  
})


#### Batch ASE = TRUE ####
test_that("Batch ASE = TRUE", {
  
  # data("USeconomic")
  data = as.data.frame(USeconomic)
  colnames(data) = c("logM1", "logGNP", "rs", "rl")
  
  lag.max = 10
  
  models = list("AIC None" = list(select = "aic", trend_type = "none", lag.max = lag.max),
                "AIC Trend"    = list(select = "aic", trend_type = "trend", lag.max = lag.max),
                "AIC Both" = list(select = "aic", trend_type = "both", lag.max = lag.max),
                "BIC None" = list(select = "bic", trend_type = "none", lag.max = lag.max),
                "BIC Trend"    = list(select = "bic", trend_type = "trend", lag.max = lag.max),
                "BIC Both" = list(select = "bic", trend_type = "both", lag.max = lag.max)
  )
  var_interest = 'logGNP'
  
  mdl_build = ModelBuildMultivariateVAR$new(data = data, var_interest = var_interest,
                                            mdl_list = models, verbose = 0)
  
  
  recommendations = mdl_build$get_recommendations()
  mdl_build$build_recommended_models()
  
  # Get only user defined models
  models = mdl_build$get_final_models(subset = 'u')
  
  #### With sliding ASE = TRUE
  
  for (name in names(models)){
    models[[name]][['sliding_ase']] = TRUE
  }
  
  batch_size = 38
  n.ahead = 2
  
  #### With n_step.ahead = TRUE (Default)              
  mdl_compare = ModelCompareMultivariateVAR$new(data = data, var_interest = var_interest,
                                                mdl_list = models, n.ahead = n.ahead, batch_size = batch_size)
  
  
  
  
  mdl_compare$get_xIC() 
  
  summary_compare = mdl_compare$summarize_build()
  #summary_compare %>% write.csv(file = "multivar_VAR_compare_summary.csv", row.names = FALSE)
  
  summary_target_file = system.file("extdata", "multivar_VAR_compare_summary.csv", package = "tswgewrapped", mustWork = TRUE)
  summary_target = read.csv(summary_target_file, header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_if(is.numeric, as.double)  # Converts integer to double to match type
  good = all.equal(summary_compare %>% dplyr::mutate_if(is.numeric, as.double), summary_target)
  testthat::expect_equal(good, TRUE)
  
  
  mdl_compare$plot_simple_forecasts()
  mdl_compare$plot_batch_forecasts(only_sliding = FALSE)
  mdl_compare$plot_batch_ases(only_sliding = FALSE)
  mdl_compare$plot_boxplot_ases()
  mdl_compare$statistical_compare()
  
})

#### "n_step.ahead = FALSE ####
test_that("n_step.ahead = FALSE", {
  
  # data("USeconomic")
  data = as.data.frame(USeconomic)
  colnames(data) = c("logM1", "logGNP", "rs", "rl")
  
  lag.max = 10
  
  models = list("AIC None" = list(select = "aic", trend_type = "none", lag.max = lag.max),
                "AIC Trend"    = list(select = "aic", trend_type = "trend", lag.max = lag.max),
                "AIC Both" = list(select = "aic", trend_type = "both", lag.max = lag.max),
                "BIC None" = list(select = "bic", trend_type = "none", lag.max = lag.max),
                "BIC Trend"    = list(select = "bic", trend_type = "trend", lag.max = lag.max),
                "BIC Both" = list(select = "bic", trend_type = "both", lag.max = lag.max)
  )
  var_interest = 'logGNP'
  
  mdl_build = ModelBuildMultivariateVAR$new(data = data, var_interest = var_interest,
                                            mdl_list = models, verbose = 0)
  
  
  recommendations = mdl_build$get_recommendations()
  mdl_build$build_recommended_models()
  
  # Get only user defined models
  models = mdl_build$get_final_models(subset = 'u')
  
  batch_size = 38
  n.ahead = 2
  
  #### With n_step.ahead = FALSE
  mdl_compare = ModelCompareMultivariateVAR$new(data = data, var_interest = var_interest,
                                                mdl_list = models, n.ahead = n.ahead, batch_size = batch_size,
                                                step_n.ahead = FALSE)
  
  forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
  
  summary = forecasts %>%
    dplyr::group_by(Model) %>%
    dplyr::summarise(MeanForecast = mean(f, na.rm = TRUE),
                     MeanLL = mean(ll, na.rm = TRUE),
                     MeanUL = mean(ul, na.rm = TRUE)
    )
  
  
})


#### Subset of data ####
test_that("Models that used only subset of data", {
  
  # data("USeconomic")
  # data = as.data.frame(USeconomic)
  
  data = USeconomic
  
  var_interest = 'logGNP'
  subset_cols = c("logM1", "logGNP")
  
  varfit1 = vars::VAR(data, p = 3, type = "none")
  varfit2 = vars::VAR(data %>% dplyr::select(!!subset_cols), p = 3, type = "none")
  
  # Get only user defined models
  models = list("All Data" = list(varfit = varfit1, sliding_ase = FALSE),
                "Subset Data" = list(varfit = varfit2, sliding_ase = FALSE)
                )
  
  #### With sliding ASE = FALSE
  n.ahead = 2
  
  #### With n_step.ahead = TRUE (Default)              
  mdl_compare = ModelCompareMultivariateVAR$new(data = data, var_interest = var_interest,
                                                mdl_list = models, n.ahead = n.ahead, verbose = 0)
  
  ## Compute ASE values from object
  ASEs = mdl_compare$get_tabular_metrics(ases = TRUE)
  
  ASE_all_data  = ASEs %>% dplyr::filter(Model == "All Data") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_subset_data = ASEs %>% dplyr::filter(Model == "Subset Data") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  
  ## Compute Forecasts, Upper Limits and Lower Limits from object
  forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
  
  summary = forecasts %>% 
    dplyr::group_by(Model) %>% 
    dplyr::summarise(MeanForecast = mean(f, na.rm = TRUE),
                     MeanLL = mean(ll, na.rm = TRUE),
                     MeanUL = mean(ul, na.rm = TRUE)
    )
  
  meanForecast_all_data = summary %>% dplyr::filter(Model == "All Data") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_all_data = summary %>% dplyr::filter(Model == "All Data") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_all_data = summary %>% dplyr::filter(Model == "All Data") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_subset_data = summary %>% dplyr::filter(Model == "Subset Data") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_subset_data = summary %>% dplyr::filter(Model == "Subset Data") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_subset_data = summary %>% dplyr::filter(Model == "Subset Data") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  
  ## Compute values computed manually to how the object calculates it
  n = nrow(data)
  train_data = data %>% dplyr::filter(dplyr::row_number() <= (n - n.ahead))
  test_data = data %>%  dplyr::filter(dplyr::row_number() > (n - n.ahead))
  
  train_data_subset = train_data %>% dplyr::select(!!subset_cols)
  test_data_subset = test_data %>% dplyr::select(!!subset_cols)
  
  trend_types = rep(c("none"),2)
  ks = c(3,3)
  train_datasets = list(train_data, train_data_subset)
  test_datasets = list(test_data, test_data_subset)
  
  ASEs = c()
  mean_forecasts = c()
  mean_lls = c()
  mean_uls = c()
  
  for (i in seq_along(trend_types)){
    
    trend_type = trend_types[i]
    k = ks[i]
    
    ## VAR Model
    varfit = vars::VAR(train_datasets[[i]], p=k, type=trend_type)
    
    ## Predictions
    preds = stats::predict(varfit, n.ahead=n.ahead)
    
    results = preds$fcst[[var_interest]] %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(Time = seq(n-n.ahead+1,n,1)) 
    
    ## ASE
    data_all = data %>% 
      dplyr::mutate(Time = dplyr::row_number())
    
    ASE_data = data_all %>% 
      dplyr::full_join(results, by = "Time") %>% 
      na.omit()
    
    ASE = mean((ASE_data[[var_interest]] - ASE_data$fcst)^2, na.rm = TRUE)
    ASEs = c(ASEs, ASE)
    
    mean_forecast = mean(ASE_data$fcst, na.rm = TRUE)
    mean_ll = mean(ASE_data$lower, na.rm = TRUE)
    mean_ul = mean(ASE_data$upper, na.rm = TRUE)
    
    mean_forecasts = c(mean_forecasts, mean_forecast)
    mean_lls = c(mean_lls, mean_ll)
    mean_uls = c(mean_uls, mean_ul)
    
  }
  
  ## Compare ASE Values
  testthat::expect_equal(ASE_all_data, ASEs[1])
  testthat::expect_equal(ASE_subset_data, ASEs[2])
  
  ## Compare Forecast, Upper Limit and Lower Limits
  testthat::expect_equal(meanForecast_all_data, mean_forecasts[1])
  testthat::expect_equal(meanForecast_subset_data, mean_forecasts[2])
 
  ## Compare Lower Limits
  testthat::expect_equal(meanLL_all_data, mean_lls[1])
  testthat::expect_equal(meanLL_subset_data, mean_lls[2])

  ## Compare Upper Limits
  testthat::expect_equal(meanUL_all_data, mean_uls[1])
  testthat::expect_equal(meanUL_subset_data, mean_uls[2])

})


