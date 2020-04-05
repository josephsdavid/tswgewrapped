## TODO: Write all unit tests

test_that("Random Parallel", {
  # http://r-pkgs.had.co.nz/tests.html
  # skip_on_cran()

  
  # # Load Data
  file = system.file("extdata", "USeconomic.csv", package = "tswgewrapped", mustWork = TRUE)
  USeconomic = read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  names(USeconomic) = gsub("[(|)]", "", colnames(USeconomic))
  data = USeconomic

  # library(caret)
  # 
  # # Random Parallel
  # model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 2,
  #                                  search = 'random',
  #                                  grid = NA, tuneLength = 2,
  #                                  batch_size = 132, h = 2,
  #                                  parallel = TRUE,
  #                                  seed = 1,
  #                                  verbose = 1)
  # 
  # model$summarize_hyperparam_results()
  # model$plot_hyperparam_results()
  # 
  # model$summarize_best_hyperparams()
  # model$summarize_build()
  # 
  # caret_model = model$get_final_models(subset = 'a')
  # # saveRDS(caret_model, "caret_model_batch_ase.rds")
  
  # Load already saved model
  file = system.file("extdata", "caret_model_batch_ase.rds", package = "tswgewrapped", mustWork = TRUE)
  caret_model = readRDS(file)
  
  mdl_compare = ModelCompareNNforCaret$new(data = data, var_interest = 'logGNP',
                                           mdl_list = caret_model,
                                           verbose = 1)
  
  ases = mdl_compare$get_tabular_metrics()
  # write.csv(ases, file = "caret_nnfor_ases.csv", row.names = FALSE)
  # Load target data
  ases_file = system.file("extdata", "caret_nnfor_ases.csv", package = "tswgewrapped", mustWork = TRUE)
  ases_target = read.csv(ases_file, header = TRUE, stringsAsFactors = FALSE)
  good1 = all.equal(as.data.frame(ases), ases_target %>% dplyr::mutate_if(is.numeric, as.double))
  testthat::expect_equal(good1, TRUE)
  
  forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
  # write.csv(forecasts, file = "caret_nnfor_forecasts.csv", row.names = FALSE)
  forecasts_file = system.file("extdata", "caret_nnfor_forecasts.csv", package = "tswgewrapped", mustWork = TRUE)
  forecasts_target = read.csv(forecasts_file, header = TRUE, stringsAsFactors = FALSE)
  good2 = all.equal(as.data.frame(forecasts), forecasts_target %>% dplyr::mutate_if(is.numeric, as.double))
  testthat::expect_equal(good2, TRUE)
  
  mdl_compare$plot_boxplot_ases()
  
  result = mdl_compare$statistical_compare()
  pval = summary(result)[[1]]$`Pr(>F)`[1]
  testthat::expect_equal(round(pval,6), 0.591116)
  
  mdl_compare$plot_batch_forecasts() 
  mdl_compare$plot_batch_ases() 
  mdl_compare$plot_simple_forecasts()

})
