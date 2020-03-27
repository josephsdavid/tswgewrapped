#### Model Compare Multivariate Class ####
test_that("ModelCompareMultivariateVAR", {

  data = as.data.frame(USeconomic)
  colnames(data) = c("logM1", "logGNP", "rs", "rl")
  
  lag.max = 10
  
  models = list("VARS AIC No Trend A" = list(select = "aic", trend_type = "none", lag.max = lag.max, sliding_ase = FALSE),
                "VARS AIC Trend A"    = list(select = "aic", trend_type = "trend", lag.max = lag.max, sliding_ase = FALSE),
                "VARS AIC Const + Trend A" = list(select = "aic", trend_type = "both", lag.max = lag.max, sliding_ase = FALSE),
                "VARS BIC No Trend A" = list(select = "bic", trend_type = "none", lag.max = lag.max, sliding_ase = FALSE),
                "VARS BIC Trend A"    = list(select = "bic", trend_type = "trend", lag.max = lag.max, sliding_ase = FALSE),
                "VARS BIC Const + Trend A" = list(select = "bic", trend_type = "both", lag.max = lag.max, sliding_ase = FALSE)
                # "VARS AIC No Trend B" = list(select = "aic", trend_type = "none", lag.max = lag.max, sliding_ase = TRUE),
                # "VARS AIC Trend B"    = list(select = "aic", trend_type = "trend", lag.max = lag.max, sliding_ase = TRUE),
                # "VARS AIC Const + Trend B" = list(select = "aic", trend_type = "both", lag.max = lag.max, sliding_ase = TRUE),
                # "VARS BIC No Trend B" = list(select = "bic", trend_type = "none", lag.max = lag.max, sliding_ase = TRUE),
                # "VARS BIC Trend B"    = list(select = "bic", trend_type = "trend", lag.max = lag.max, sliding_ase = TRUE),
                # "VARS BIC Const + Trend B" = list(select = "bic", trend_type = "both", lag.max = lag.max, sliding_ase = TRUE)
                )
  
  n.ahead = 2
  batch_size = 50
  var_interest = 'logGNP'

  #### With n_step.ahead = TRUE (Default)              
  mdl_compare = ModelCompareMultivariateVAR$new(data = data, var_interest = var_interest,
                                             mdl_list = models, n.ahead = n.ahead, batch_size = batch_size)
  
  
  mdl_compare$get_xIC() ## TODO: May change this later
  
  
  mdl_compare$plot_simple_forecasts()
  
  #mdl_compare$plot_batch_forecasts(only_sliding = TRUE)
  mdl_compare$plot_batch_forecasts(only_sliding = FALSE)
  #mdl_compare$plot_batch_ases(only_sliding = TRUE)
  mdl_compare$plot_batch_ases(only_sliding = FALSE)
  mdl_compare$plot_histogram_ases()
  mdl_compare$statistical_compare()
  
  # mdl_compare$plot_multiple_realizations(n.realizations = 4, seed = 100, scales = 'free_y')
  
  ## Compute ASE values from object
  ASEs = mdl_compare$get_tabular_metrics(ases = TRUE)
  
  ASE_aic_none_a  = ASEs %>% dplyr::filter(Model == "VARS AIC No Trend A") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_aic_trend_a = ASEs %>% dplyr::filter(Model == "VARS AIC Trend A") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_aic_both_a  = ASEs %>% dplyr::filter(Model == "VARS AIC Const + Trend A") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_bic_none_a  = ASEs %>% dplyr::filter(Model == "VARS BIC No Trend A") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_bic_trend_a = ASEs %>% dplyr::filter(Model == "VARS BIC Trend A") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  ASE_bic_both_a  = ASEs %>% dplyr::filter(Model == "VARS BIC Const + Trend A") %>% dplyr::select(ASE) %>%  purrr::pluck(1)
  
  ## Compute Forecasts, Upper Limits and Lower Limits from object
  forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
  
  summary = forecasts %>% 
    dplyr::group_by(Model) %>% 
    dplyr::summarise(MeanForecast = mean(f, na.rm = TRUE),
                     MeanLL = mean(ll, na.rm = TRUE),
                     MeanUL = mean(ul, na.rm = TRUE)
    )
  
  meanForecast_aic_none_a = summary %>% dplyr::filter(Model == "VARS AIC No Trend A") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_aic_none_a = summary %>% dplyr::filter(Model == "VARS AIC No Trend A") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_aic_none_a = summary %>% dplyr::filter(Model == "VARS AIC No Trend A") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_aic_trend_a = summary %>% dplyr::filter(Model == "VARS AIC Trend A") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_aic_trend_a = summary %>% dplyr::filter(Model == "VARS AIC Trend A") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_aic_trend_a = summary %>% dplyr::filter(Model == "VARS AIC Trend A") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_aic_both_a = summary %>% dplyr::filter(Model == "VARS AIC Const + Trend A") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_aic_both_a = summary %>% dplyr::filter(Model == "VARS AIC Const + Trend A") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_aic_both_a = summary %>% dplyr::filter(Model == "VARS AIC Const + Trend A") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_bic_none_a = summary %>% dplyr::filter(Model == "VARS BIC No Trend A") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_bic_none_a = summary %>% dplyr::filter(Model == "VARS BIC No Trend A") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_bic_none_a = summary %>% dplyr::filter(Model == "VARS BIC No Trend A") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_bic_trend_a = summary %>% dplyr::filter(Model == "VARS BIC Trend A") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_bic_trend_a = summary %>% dplyr::filter(Model == "VARS BIC Trend A") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_bic_trend_a = summary %>% dplyr::filter(Model == "VARS BIC Trend A") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  meanForecast_bic_both_a = summary %>% dplyr::filter(Model == "VARS BIC Const + Trend A") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1)
  meanLL_bic_both_a = summary %>% dplyr::filter(Model == "VARS BIC Const + Trend A") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1)
  meanUL_bic_both_a = summary %>% dplyr::filter(Model == "VARS BIC Const + Trend A") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1)
  
  
  
  ## Compute values computed manually to how the object calculates it
  n = nrow(data)
  train_data = data %>% dplyr::filter(dplyr::row_number() <= (n - n.ahead))
  test_data = data %>%  dplyr::filter(dplyr::row_number() > (n - n.ahead))

  trend_types = c("none", "trend", "both")
  selections = c("AIC(n)", "SC(n)")
  
  ASEs = c()
  mean_forecasts = c()
  mean_lls = c()
  mean_uls = c()
  
  for (selection in selections){
    for (trend_type in trend_types){
      ## VARselect
      vselect = vars::VARselect(data, lag.max = lag.max, type = trend_type, season = NULL, exogen = NULL)
      vselect # Gives AIC values for various K values
      k = vselect$selection[[selection]]  
      
      ## VAR Model
      lsfit = vars::VAR(train_data, p=k, type=trend_type)
      
      ## Predictions
      preds = stats::predict(lsfit, n.ahead=n.ahead)
      
      results = preds$fcst[[var_interest]] %>% 
        tibble::as.tibble() %>% 
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
  
  
  # meanForecast_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  # meanLL_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  # meanUL_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)
  # 
  # meanForecast_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  # meanLL_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  # meanUL_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)
  # 
  # meanForecast_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  # meanLL_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  # meanUL_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)
  # 
  # expect_equal(meanForecast_wg_modelB, 5.75461)
  # expect_equal(meanForecast_pz_modelB, 5.697653)
  # expect_equal(meanForecast_bx_modelB, 5.764886)
  # 
  # expect_equal(meanLL_wg_modelB, 5.627753)
  # expect_equal(meanLL_pz_modelB, 5.573369)
  # expect_equal(meanLL_bx_modelB, 5.653335)
  # 
  # expect_equal(meanUL_wg_modelB, 5.881467)
  # expect_equal(meanUL_pz_modelB, 5.821938)
  # expect_equal(meanUL_bx_modelB, 5.876438)
  
  
  # # Generated White Noise 
  # wn = tswge::gen.arma.wge(n = 200, sn = 101, plot = FALSE)
  # 
  # # Using p = 1 since I need to pass that to the ModelCompareUnivariate to make it work
  # k24 = tswge::ljung.wge(wn, K = 24, p = 1)  
  # k48 = tswge::ljung.wge(wn, K = 48, p = 1)
  # 
  # models = list("Model 1" = list(phi = 0.5, res = wn, sliding_ase = FALSE))  # Hypothetical Model
  # 
  # mdl_compare = ModelCompareUnivariate$new(data = wn, mdl_list = models, n.ahead = 10)
  # table = mdl_compare$evaluate_residuals()
  # 
  # expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(pval) %>% purrr::pluck(1), k24$pval)
  # expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(pval) %>% purrr::pluck(1), k48$pval)
  # expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(Decision) %>% purrr::pluck(1), "FTR NULL")
  # expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(Decision) %>% purrr::pluck(1), "FTR NULL")
  
})
