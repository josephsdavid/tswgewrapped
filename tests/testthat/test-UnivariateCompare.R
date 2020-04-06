#### Basic Tests ####
test_that("ModelCompareUnivariate", {

  # Woodward Gray Airline Model
  phi_wg = c(-0.36, -0.05, -0.14, -0.11, 0.04, 0.09, -0.02, 0.02, 0.17, 0.03, -0.10, -0.38)
  d_wg = 1
  s_wg = 12
  
  # Parzen Model
  phi_pz = c(0.74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.38, -0.2812)
  s_pz = 12
  
  # Box Model
  d_bx = 1
  s_bx = 12  
  theta_bx =  c(0.40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.60, -0.24)
  
  models = list("Woodward Gray Model A" = list(phi = phi_wg, d = d_wg, s = s_wg, vara = 0.001357009, sliding_ase = FALSE),
                "Woodward Gray Model B" = list(phi = phi_wg, d = d_wg, s = s_wg, vara = 0.001357009, sliding_ase = TRUE),
                "Parzen Model A" = list(phi = phi_pz, s = s_pz, vara = 0.002933592, sliding_ase = FALSE),
                "Parzen Model B" = list(phi = phi_pz, s = s_pz, vara = 0.002933592, sliding_ase = TRUE),
                "Box Model A" = list(theta = theta_bx, d = d_bx, s = s_bx, vara = 0.001391604, sliding_ase = FALSE),
                "Box Model B" = list(theta = theta_bx, d = d_bx, s = s_bx, vara = 0.001391604, sliding_ase = TRUE)
                )
  
  #### With n_step.ahead = TRUE (Default)              
  mdl_compare = ModelCompareUnivariate$new(data = airlog, mdl_list = models,
                                           n.ahead = 36, batch_size = 72)
  
  p = mdl_compare$plot_simple_forecasts()
  p = mdl_compare$plot_simple_forecasts(zoom = 50)
  p = mdl_compare$plot_batch_forecasts(only_sliding = TRUE)
  p = mdl_compare$plot_batch_forecasts(only_sliding = FALSE)
  p = mdl_compare$plot_batch_ases(only_sliding = TRUE)
  p = mdl_compare$plot_batch_ases(only_sliding = FALSE)
  p = mdl_compare$plot_boxplot_ases()
  mdl_compare$statistical_compare()
  
  mdl_compare$plot_multiple_realizations(n.realizations = 4, seed = 100, scales = 'free_y')
  
  ASEs = mdl_compare$get_tabular_metrics(ases = TRUE)
  
  ASE_wg_single_batch = round(ASEs %>% dplyr::filter(Model == "Woodward Gray Model A") %>% dplyr::select(ASE) %>%  purrr::pluck(1), 9)
  ASE_pz_single_batch = round(ASEs %>% dplyr::filter(Model == "Parzen Model A") %>% dplyr::select(ASE) %>%  purrr::pluck(1), 9)
  ASE_bx_single_batch = round(ASEs %>% dplyr::filter(Model == "Box Model A") %>% dplyr::select(ASE) %>%  purrr::pluck(1), 9)
  
  expect_equal(ASE_wg_single_batch, 0.004185726)
  expect_equal(ASE_pz_single_batch, 0.012526356)
  expect_equal(ASE_bx_single_batch, 0.006903242)
  
  forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
  
  
  summary = forecasts %>% 
    dplyr::group_by(Model) %>% 
    dplyr::summarise(MeanForecast = mean(f, na.rm = TRUE),
              MeanLL = mean(ll, na.rm = TRUE),
              MeanUL = mean(ul, na.rm = TRUE)
    )
  

  meanForecast_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  meanLL_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  meanUL_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)
  
  meanForecast_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  meanLL_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  meanUL_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)
  
  meanForecast_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  meanLL_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  meanUL_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)
  
  expect_equal(meanForecast_wg_modelB, 5.767088)
  expect_equal(meanForecast_pz_modelB, 5.588596)
  expect_equal(meanForecast_bx_modelB, 5.736360)
  
  expect_equal(meanLL_wg_modelB, 5.477559)
  expect_equal(meanLL_pz_modelB, 5.348760)
  expect_equal(meanLL_bx_modelB, 5.502834)
  
  expect_equal(meanUL_wg_modelB, 6.056617)
  expect_equal(meanUL_pz_modelB, 5.828432)
  expect_equal(meanUL_bx_modelB, 5.969887)
  
  
  #### With n_step.ahead = FALSE
  
  mdl_compare = ModelCompareUnivariate$new(data = airlog, mdl_list = models,
                                           n.ahead = 36, batch_size = 72, step_n.ahead = FALSE)


  forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)

  summary = forecasts %>%
    dplyr::group_by(Model) %>%
    dplyr::summarise(MeanForecast = mean(f, na.rm = TRUE),
              MeanLL = mean(ll, na.rm = TRUE),
              MeanUL = mean(ul, na.rm = TRUE)
    )


  meanForecast_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  meanLL_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  meanUL_wg_modelB = round(summary %>% dplyr::filter(Model == "Woodward Gray Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)

  meanForecast_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  meanLL_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  meanUL_pz_modelB = round(summary %>% dplyr::filter(Model == "Parzen Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)

  meanForecast_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanForecast) %>%  purrr::pluck(1), 6)
  meanLL_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanLL) %>%  purrr::pluck(1), 6)
  meanUL_bx_modelB = round(summary %>% dplyr::filter(Model == "Box Model B") %>% dplyr::select(MeanUL) %>%  purrr::pluck(1), 6)

  expect_equal(meanForecast_wg_modelB, 5.75461)
  expect_equal(meanForecast_pz_modelB, 5.697653)
  expect_equal(meanForecast_bx_modelB, 5.764886)

  expect_equal(meanLL_wg_modelB, 5.627753)
  expect_equal(meanLL_pz_modelB, 5.573369)
  expect_equal(meanLL_bx_modelB, 5.653335)

  expect_equal(meanUL_wg_modelB, 5.881467)
  expect_equal(meanUL_pz_modelB, 5.821938)
  expect_equal(meanUL_bx_modelB, 5.876438)
  
  
  # Generated White Noise 
  wn = tswge::gen.arma.wge(n = 200, sn = 101, plot = FALSE)
  
  # Using p = 1 since I need to pass that to the ModelCompareUnivariate to make it work
  k24 = tswge::ljung.wge(wn, K = 24, p = 1)  
  k48 = tswge::ljung.wge(wn, K = 48, p = 1)
  
  models = list("Model 1" = list(phi = 0.5, res = wn, sliding_ase = FALSE))  # Hypothetical Model
  
  mdl_compare = ModelCompareUnivariate$new(data = wn, mdl_list = models, n.ahead = 10)
  table = mdl_compare$evaluate_residuals()
  
  expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(pval) %>% purrr::pluck(1), k24$pval)
  expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(pval) %>% purrr::pluck(1), k48$pval)
  expect_equal(table %>% dplyr::filter(K == 24) %>% dplyr::select(Decision) %>% purrr::pluck(1), "FTR NULL")
  expect_equal(table %>% dplyr::filter(K == 48) %>% dplyr::select(Decision) %>% purrr::pluck(1), "FTR NULL")
  
})

#### Add Models ####
test_that("ModelCompareUnivariate - Add Models", {
  
  # Woodward Gray Airline Model
  phi_wg = c(-0.36, -0.05, -0.14, -0.11, 0.04, 0.09, -0.02, 0.02, 0.17, 0.03, -0.10, -0.38)
  d_wg = 1
  s_wg = 12
  
  # Parzen Model
  phi_pz = c(0.74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.38, -0.2812)
  s_pz = 12
  
  # Box Model
  d_bx = 1
  s_bx = 12  
  theta_bx =  c(0.40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.60, -0.24)
  
  models = list("Woodward Gray Model A" = list(phi = phi_wg, d = d_wg, s = s_wg, vara = 0.001357009, sliding_ase = FALSE),
                "Parzen Model A" = list(phi = phi_pz, s = s_pz, vara = 0.002933592, sliding_ase = FALSE),
                "Box Model A" = list(theta = theta_bx, d = d_bx, s = s_bx, vara = 0.001391604, sliding_ase = FALSE)
                )
  
  #### With n_step.ahead = TRUE (Default)              
  mdl_compare = ModelCompareUnivariate$new(data = airlog, mdl_list = models,
                                           n.ahead = 36)
  
  models = list("Woodward Gray Model B" = list(phi = phi_wg, d = d_wg, s = s_wg, vara = 0.001357009, sliding_ase = TRUE),
                "Parzen Model B" = list(phi = phi_pz, s = s_pz, vara = 0.002933592, sliding_ase = TRUE),
                "Box Model B" = list(theta = theta_bx, d = d_bx, s = s_bx, vara = 0.001391604, sliding_ase = TRUE)
  )
  
  # Warning for setting batch size
  expect_warning(mdl_compare$add_models(mdl_list = models),
                 "You have provided models that require sliding ASE calculations, but the batch size has been set to NA. Setting the batch size to the length of the realization.")

  # Error for model name uniqueness
  expect_error(mdl_compare$add_models(mdl_list = models, batch_size = 72),
               "The model names above already exist in this comparison object. Please provide unique names.")
  
  mdl_compare$set_batch_size(72)
  # Check all functions after data manipulation
  p = mdl_compare$plot_simple_forecasts()
  p = mdl_compare$plot_batch_forecasts(only_sliding = TRUE)
  p = mdl_compare$plot_batch_forecasts(only_sliding = FALSE)
  p = mdl_compare$plot_batch_ases(only_sliding = TRUE)
  p = mdl_compare$plot_batch_ases(only_sliding = FALSE)
  p = mdl_compare$plot_boxplot_ases()
  mdl_compare$statistical_compare()
  
  # Remove models
  mdl_compare$remove_models(mdl_names = names(models))
  
  # Add them back with correct batch size
  mdl_compare$add_models(mdl_list = models, batch_size = 72)
  
  # Check all functions after data manipulation
  p = mdl_compare$plot_simple_forecasts()
  p = mdl_compare$plot_batch_forecasts(only_sliding = TRUE)
  p = mdl_compare$plot_batch_forecasts(only_sliding = FALSE)
  p = mdl_compare$plot_batch_ases(only_sliding = TRUE)
  p = mdl_compare$plot_batch_ases(only_sliding = FALSE)
  p = mdl_compare$plot_boxplot_ases()
  mdl_compare$statistical_compare()
  
  
})



