#### Model Build Multivariate Class ####
test_that("ModelBuildMultivariateVAR", {

  data = as.data.frame(USeconomic)
  colnames(data) = c("logM1", "logGNP", "rs", "rl")
  
  lag.max = 10
  
  models = list("VARS AIC No Trend A" = list(select = "aic", trend_type = "none", lag.max = lag.max),
                "VARS AIC Trend A"    = list(select = "aic", trend_type = "trend", lag.max = lag.max),
                "VARS AIC Const + Trend A" = list(select = "aic", trend_type = "both", lag.max = lag.max),
                "VARS BIC No Trend A" = list(select = "bic", trend_type = "none", lag.max = lag.max),
                "VARS BIC Trend A"    = list(select = "bic", trend_type = "trend", lag.max = lag.max),
                "VARS BIC Const + Trend A" = list(select = "bic", trend_type = "both", lag.max = lag.max)
                )
  var_interest = 'logGNP'

  mdl_build = ModelBuildMultivariateVAR$new(data = data, var_interest = var_interest,
                                            mdl_list = models, verbose = 1)

  
  mdl_build$summarize_build()
  
  
  
})
