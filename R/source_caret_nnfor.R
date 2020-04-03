# This file contains the code that needs to be sourced if you want to use the package with caret

#### Adding custom model to caret ####
nnfor_caret <- list(type = c("Regression"),
                   library = "nnfor",
                   loop = NULL)

prm <- data.frame(parameter = c("reps", "hd", "allow.det.season"),
                  class = c(rep("numeric", 2), rep("logical", 1)),
                  label = c("reps", "hd","allow.det.season"))

nnfor_caret$parameters <- prm

nnforGrid <- function(x, y, len = NULL, search = "grid") {
  if (search == 'grid'){
    rvGrid = expand.grid(reps = c(20, 50),
                         hd = c(1, 5),
                         allow.det.season = c(TRUE))
  }
  else{
    ## For random search, generate random values for them
    rvGrid = data.frame(reps = sample(10:25, size = len, replace = TRUE),
                        hd = sample(1:5, size = len, replace = TRUE),
                        allow.det.season = sample(c(TRUE, FALSE), size = len, replace = TRUE))
  }
  return(rvGrid)
}

nnfor_caret$grid <- nnforGrid

nnforFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  # library(nnfor)

  ## https://stackoverflow.com/questions/13353847/how-to-expand-an-ellipsis-argument-without-evaluating-it-in-r
  # print(match.call(expand.dots = FALSE)$`...`)
  
  # # https://stats.stackexchange.com/questions/89171/help-requested-with-using-custom-model-in-caret-package
  xreg = as.data.frame(x)

  mod = nnfor::mlp(y = stats::ts(y),
                   xreg = xreg,
                   reps = param$reps,
                   hd = param$hd,
                   allow.det.season = param$allow.det.season,
                   ...)

  # We need the xref values in the predict function, so we add it here so that we can access it during prediction  
  mod$xreg = xreg

  return(mod)

}

nnfor_caret$fit <- nnforFit

# Setting prob to NULL since we dont need it
# https://stackoverflow.com/questions/59181634/error-some-required-components-are-missing-prob
nnfor_caret = c(nnfor_caret, list(prob = NULL))

nnforPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
  
  xreg = rbind(modelFit$xreg, newdata)

  rvPred = forecast::forecast(modelFit,
                              h = nrow(newdata),
                              xreg = xreg
                              )  
  return(rvPred)
}

nnfor_caret$predict <- nnforPred


#### Custom Metrics ####

## See https://topepo.github.io/caret/model-training-and-tuning.html#metrics
caret_metric_ASE = function(data, lev = NULL, model = NULL) {
  ASE = mean((data$pred - data$obs)^2)
  RMSE = sqrt(ASE)
  return (c(RMSE = RMSE, ASE = ASE))
}


