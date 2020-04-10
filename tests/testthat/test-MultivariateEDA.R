#### Multivariate EDA Class ####
test_that("Multivariate EDA Plotting", {
  
  eda = MultivariateEDA$new(data = USeconomic, var_interest = "logGNP")
  
  eda$plot_data()
  
  ccf_data = eda$plot_ccf_analysis()
  expect_equal(round(sum(ccf_data$max_ccf_value),6), 2.225412)
  
  eda$plot_scatterplots()
})
