test_that("Combine Models", {
  # # Load Data
  file = system.file("extdata", "USeconomic.csv", package = "tswgewrapped", mustWork = TRUE)
  USeconomic = read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  names(USeconomic) = gsub("[(|)]", "", colnames(USeconomic))
  data = USeconomic
  
  var_interest = 'logGNP'
  batch_size = 130
  n.ahead = 2
  
  data_train = data %>% dplyr::slice(1:(dplyr::n()-n.ahead))
  data_test = data %>% dplyr::slice((dplyr::n()-n.ahead), dplyr::n())
  
  #### 1.0 Build and Compare Models ####
  #### 1.1 Univariate ####
  
  models = list("Univar A" = list(phi = 0.9, d = 1, s = 0, sliding_ase = TRUE),
                "Univar B" = list(phi = 0.9, d = 1, s = 4, sliding_ase = TRUE),
                "Univar C" = list(phi = 0.9, d = 1, s = 4, sliding_ase = FALSE)
                )
  
  mdl_compare_uni = ModelCompareUnivariate$new(data = data_train, var_interest = var_interest, mdl_list = models,
                                               n.ahead = n.ahead, batch_size = batch_size)
  
  
  #### 1.2 VAR ####
  
  lag.max = 10
  
  models = list("AIC None" = list(select = "aic", trend_type = "none", lag.max = lag.max),
                "AIC Trend" = list(select = "aic", trend_type = "trend", lag.max = lag.max),
                "AIC Both" = list(select = "aic", trend_type = "both", lag.max = lag.max))
  
  
  
  mdl_build_var = ModelBuildMultivariateVAR$new(data = data_train, var_interest = var_interest,
                                                mdl_list = models, verbose = 0)
  
  
  mdl_build_var$build_recommended_models()
  models = mdl_build_var$get_final_models(subset = 'r')
  
  # Setup Models to be compared 
  
  #### With sliding ASE = TRUE
  for (name in names(models)){
    models[[name]][['sliding_ase']] = TRUE
  }
  
  # Initialize the ModelCompareMultivariateVAR object
  mdl_compare_var = ModelCompareMultivariateVAR$new(data = data_train, var_interest = var_interest,
                                                    mdl_list = models, n.ahead = n.ahead, batch_size = batch_size, verbose = 0)
  
  
  
  #### 1.3 NNFOR::mlp() caret ####
  
  # library(caret)
  # 
  # # Random Parallel
  # model = ModelBuildNNforCaret$new(data = data_train, var_interest = "logGNP", m = 4,
  #                                  search = 'random',
  #                                  grid = NA, tuneLength = 2,
  #                                  batch_size = batch_size, h = n.ahead,
  #                                  parallel = TRUE,
  #                                  seed = 1,
  #                                  verbose = 1)
  # 
  # model$summarize_hyperparam_results()
  # caret_model = model$get_final_models(subset = 'a')
  # # saveRDS(caret_model, "caret_model_batch_ase_train.rds")
  
  file = system.file("extdata", "caret_model_batch_ase_train.rds", package = "tswgewrapped", mustWork = TRUE)
  caret_model = readRDS(file)
  
  # Initialize the ModelCompareMultivariateVAR object
  mdl_compare_mlp = ModelCompareNNforCaret$new(data = data_train, var_interest = var_interest,
                                               mdl_list = caret_model,
                                               verbose = 1)
  
  
  #### 2.0 Combine all models ####
  
  mdl_combine = ModelCombine$new(data = data_train, var_interest = "logGNP",
                                 uni_models = mdl_compare_uni, var_models = mdl_compare_var, mlp_models = mdl_compare_mlp,
                                 verbose = 1)
  
  ## Var Interest
  testthat::expect_equal(mdl_combine$get_var_interest(), "logGNP")
  
  ## Tabular Metrics ASEs
  ases1 = mdl_combine$get_tabular_metrics()
  # write.csv(ases1, file = "mdl_combine_ases1_train.csv", row.names = FALSE)
  ases1_file = system.file("extdata", "mdl_combine_ases1_train.csv", package = "tswgewrapped", mustWork = TRUE)
  ases1_target = read.csv(ases1_file, header = TRUE, stringsAsFactors = FALSE)
  good1 = all.equal(as.data.frame(ases1), ases1_target %>% dplyr::mutate_if(is.numeric, as.double))
  testthat::expect_equal(good1, TRUE)
  
  ases2 = mdl_combine$get_tabular_metrics(only_sliding = TRUE)
  # write.csv(ases2, file = "mdl_combine_ases2_train.csv", row.names = FALSE)
  ases2_file = system.file("extdata", "mdl_combine_ases2_train.csv", package = "tswgewrapped", mustWork = TRUE)
  ases2_target = read.csv(ases2_file, header = TRUE, stringsAsFactors = FALSE)
  good2 = all.equal(as.data.frame(ases2), ases2_target %>% dplyr::mutate_if(is.numeric, as.double))
  testthat::expect_equal(good2, TRUE)
  
  ## Tabular Metrics Forecasts
  forecasts1 = mdl_combine$get_tabular_metrics(ases = FALSE)
  # write.csv(forecasts1, file = "mdl_combine_forecasts1_train.csv", row.names = FALSE)
  forecasts1_file = system.file("extdata", "mdl_combine_forecasts1_train.csv", package = "tswgewrapped", mustWork = TRUE)
  forecasts1_target = read.csv(forecasts1_file, header = TRUE, stringsAsFactors = FALSE)
  good3 = all.equal(as.data.frame(forecasts1), forecasts1_target %>% dplyr::mutate_if(is.numeric, as.double))
  testthat::expect_equal(good3, TRUE)
  
  forecasts2 = mdl_combine$get_tabular_metrics(ases = FALSE, only_sliding = TRUE)
  # write.csv(forecasts2, file = "mdl_combine_forecasts2_train.csv", row.names = FALSE)
  forecasts2_file = system.file("extdata", "mdl_combine_forecasts2_train.csv", package = "tswgewrapped", mustWork = TRUE)
  forecasts2_target = read.csv(forecasts2_file, header = TRUE, stringsAsFactors = FALSE)
  good4 = all.equal(as.data.frame(forecasts2), forecasts2_target %>% dplyr::mutate_if(is.numeric, as.double))
  testthat::expect_equal(good4, TRUE)
  
  ## Plotting (just make sure it runs for now)
  mdl_combine$plot_batch_ases()
  mdl_combine$plot_batch_ases(only_sliding = FALSE)
  mdl_combine$plot_boxplot_ases()
  mdl_combine$plot_batch_forecasts()
  mdl_combine$plot_batch_forecasts(only_sliding = FALSE)
  
  ## Statistical Comparison
  comparison = mdl_combine$statistical_compare()
  testthat::expect_equal(round(summary(comparison)[[1]][['Pr(>F)']][1],6), 0.372514)
  
  ## Forecast Compariaon
  data_test[var_interest]
  mdl_compare_uni$plot_simple_forecasts(lastn = FALSE)
  mdl_compare_var$plot_simple_forecasts(lastn = FALSE)
  mdl_compare_mlp$plot_simple_forecasts(lastn = FALSE, newxreg = data_test %>% dplyr::select(-!!var_interest), zoom = 5) 

})
