## TODO: Write all unit tests
## TODO: Run in series and parallel with random grid and random and with and without user defined grid and tuneLength
## TODO: Then compare above to manual test in nnfor
## TODO: Check reproducibility of 2 runs with caret
## TODO: Check univariate test with nnfor

## TODO: For now, we are predicting with the actual future values of xreg.
##       In the future, change this to use forecasted values of xreg
##       This can probably be achieved by passing xreg_name_predicted in the data and adjusting the source_caret_nnfor file accordingly

test_that("Random Serial", {
  # http://r-pkgs.had.co.nz/tests.html
  # TODO: Disable test before submitting to CRAN
  # testthat::skip_on_cran()
  
  # Load Data
  data = USeconomic

  model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 4,
                                   search = 'random',
                                   grid = NA, tuneLength = 2,
                                   batch_size = 136, h = 2,
                                   parallel = TRUE,
                                   seed = 1,
                                   verbose = 1)

  model$summarize_hyperparam_results()
  model$summarize_best_hyperparams()
  model$plot_hyperparam_results()
  model$summarize_build()

  good1 = TRUE
  testthat::expect_equal(good1, TRUE)

  good2 = TRUE
  testthat::expect_equal(good2, TRUE)

})

# 
# test_that("Grid Serial", {
#   # http://r-pkgs.had.co.nz/tests.html
#   # skip_on_cran()
#   
#   # Load Data
#   data = USeconomic
#   
#   library(caret)
#   
#   nnfor_grid = expand.grid(reps = c(20),
#                            hd = c(1:2),
#                            allow.det.season = c(FALSE))
#   
#   model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 4,
#                                    search = 'grid',
#                                    grid = nnfor_grid,  # Use default grid
#                                    batch_size = 136, h = 2,
#                                    parallel = FALSE,
#                                    seed = 1,
#                                    verbose = 1)
#   
#   # # #testthat::expect_equal(good, TRUE)
# 
# })
# 
#
# test_that("Random Parallel", {
#   # http://r-pkgs.had.co.nz/tests.html
#   # skip_on_cran()
#   
#   # Load Data
#   data = USeconomic
#   
#   library(caret)
#   
#   model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 4,
#                                    search = 'random',
#                                    grid = NA, tuneLength = 2,
#                                    batch_size = 136, h = 2,
#                                    parallel = TRUE,
#                                    seed = 1,
#                                    verbose = 1)
#   
#   
#   # # #testthat::expect_equal(good, TRUE)
#   
#   
# })
#
# 
# 
# test_that("Grid Parallel", {
#   # http://r-pkgs.had.co.nz/tests.html
#   # skip_on_cran()
#   
#   # Load Data
#   data = USeconomic
#   
#   library(caret)
#   
#   model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 4,
#                                    search = 'grid',
#                                    grid = NA, # Default Grid
#                                    batch_size = 136, h = 2,
#                                    parallel = TRUE,
#                                    seed = 1,
#                                    verbose = 1)
#   
#   # # #testthat::expect_equal(good, TRUE)
#   
#   
# })
# 
# 
