
# TODO: ... may not work in the initialization. Check.
# TODO: Check if adding models after object initialization works, especially id we have asked for recommendations already

#### Non Seasonal Model ####
test_that("Non Seasonal Model", {

  data("USeconomic")
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
                                            mdl_list = models, verbose = 1)

  
  summary_build = mdl_build$summarize_build()
  recommendations = mdl_build$get_recommendations()
  
  #summary_build %>% write.csv(file = "multivar_VAR_summary.csv", row.names = FALSE)
  #recommendations %>% write.csv(file = "multivar_VAR_recommendations.csv", row.names = FALSE)
  
  # https://stackoverflow.com/questions/32328802/where-should-i-put-data-for-automated-tests-with-testthat
  # http://r-pkgs.had.co.nz/data.html#other-data
  summary_target_file = system.file("extdata", "multivar_VAR_build_no_season_summary.csv", package = "tswgewrapped", mustWork = TRUE)
  summary_target = read.csv(summary_target_file, header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_if(is.numeric, as.double)  # Converts integer to double to match type
  good = all.equal(summary_build, summary_target)
  testthat::expect_equal(good, TRUE)
  
  recommendation_target_file = system.file("extdata", "multivar_VAR_build_no_season_recommendations.csv", package = "tswgewrapped", mustWork = TRUE)
  recommendation_target = read.csv(recommendation_target_file, header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_if(is.numeric, as.double)  # Converts integer to double to match type
  good = all.equal(recommendations %>% dplyr::mutate_if(is.numeric, as.double), recommendation_target)
  testthat::expect_equal(good, TRUE)
  
  mdl_build$build_recommended_models()
  final_models = mdl_build$get_final_models()
  
  # saveRDS(final_models, "multivar_VAR_final_model_list.rds")
  
  final_model_list_file = system.file("extdata", "multivar_VAR_build_final_model_no_season_list.rds", package = "tswgewrapped", mustWork = TRUE)
  final_models_target = readRDS(final_model_list_file)
  good = all.equal(final_models, final_models_target)
  testthat::expect_equal(good, TRUE)
  
  
})

#### Seasonal Model ####
test_that("Seasonal Model", {
  
  data("USeconomic")
  data = as.data.frame(USeconomic)
  colnames(data) = c("logM1", "logGNP", "rs", "rl")
  
  lag.max = 10
  
  models = list("AIC Trend"    = list(select = "aic", trend_type = "trend", season = 3, lag.max = lag.max),
                "BIC Trend"    = list(select = "bic", trend_type = "trend", season = 4, lag.max = lag.max))
  
  var_interest = 'logGNP'
  
  mdl_build = ModelBuildMultivariateVAR$new(data = data, var_interest = var_interest,
                                            mdl_list = models, verbose = 1)
  
  summary_build = mdl_build$summarize_build()
  recommendations = mdl_build$get_recommendations()
  
  # summary_build %>% write.csv(file = "multivar_VAR_build_season_summary.csv", row.names = FALSE)
  # recommendations %>% write.csv(file = "multivar_VAR_build_season_recommendations.csv", row.names = FALSE)
  
  # https://stackoverflow.com/questions/32328802/where-should-i-put-data-for-automated-tests-with-testthat
  # http://r-pkgs.had.co.nz/data.html#other-data
  summary_target_file = system.file("extdata", "multivar_VAR_build_season_summary.csv", package = "tswgewrapped", mustWork = TRUE)
  summary_target = read.csv(summary_target_file, header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_if(is.numeric, as.double)  # Converts integer to double to match type
  good = all.equal(summary_build, summary_target)
  testthat::expect_equal(good, TRUE)
  
  recommendation_target_file = system.file("extdata", "multivar_VAR_build_season_recommendations.csv", package = "tswgewrapped", mustWork = TRUE)
  recommendation_target = read.csv(recommendation_target_file, header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_if(is.numeric, as.double)  # Converts integer to double to match type
  good = all.equal(recommendations %>% dplyr::mutate_if(is.numeric, as.double), recommendation_target)
  testthat::expect_equal(good, TRUE)
  
  mdl_build$build_recommended_models()
  final_models = mdl_build$get_final_models()
  
  # saveRDS(final_models, "multivar_VAR_build_final_model_season_list.rds")
  
  final_model_list_file = system.file("extdata", "multivar_VAR_build_final_model_season_list.rds", package = "tswgewrapped", mustWork = TRUE)
  final_models_target = readRDS(final_model_list_file)
  good = all.equal(final_models, final_models_target)
  testthat::expect_equal(good, TRUE)
  
  
})
