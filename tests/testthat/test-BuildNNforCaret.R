## TODO: Write all unit tests
## TODO: Run in series and parallel with random grid and random and with and without user defined grid and tuneLength
## TODO: Then compare above to manual test in nnfor
## TODO: Check reproducibility of 2 runs with caret

# test_that("Random Parallel", {
#   # http://r-pkgs.had.co.nz/tests.html
#   # skip_on_cran()
#   
#   # Load Data
#   data = USeconomic
#   
#   library(caret)
# 
#   # Random Parallel
#   model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 2,
#                                    search = 'random',
#                                    grid = NA, tuneLength = 2,
#                                    batch_size = 136, h = 2,
#                                    parallel = TRUE,
#                                    seed = 1,
#                                    verbose = 1)
# 
#   # # #testthat::expect_equal(good, TRUE)
# 
# })
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
#   # Grid Parallel
#   model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 2,
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
# test_that("Random Serial", {
#   # http://r-pkgs.had.co.nz/tests.html
#   # skip_on_cran()
#   
#   # Load Data
#   data = USeconomic
#   
#   library(caret)
#   
#   # Random Serial
#   model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 2,
#                                    search = 'random',
#                                    grid = NA, tuneLength = 2,
#                                    batch_size = 136, h = 2,
#                                    parallel = FALSE,
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
# test_that("Grid Serial", {
#   # http://r-pkgs.had.co.nz/tests.html
#   # skip_on_cran()
#   
#   # Load Data
#   data = USeconomic
#   
#   library(caret)
#   
#   # Grid Serial
#   nnfor_grid = expand.grid(reps = c(20),
#                            hd = c(1:2),
#                            allow.det.season = c(FALSE))
#   
#   model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 2,
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
