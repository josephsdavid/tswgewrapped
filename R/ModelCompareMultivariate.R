#' @title R6 class ModelCompareMultivariate
#' 
#' @export
ModelCompareMultivariateVAR = R6::R6Class(
  classname = "ModelCompareMultivariateVAR",
  inherit = ModelCompareBase,
  cloneable = TRUE,
  lock_objects=F,
  lock_class=F,
  
  #### Public Methods ----
  public=list(
    #### Constructor ----
    
    #' @description 
    #' Initialize an object to compare several Univatiate Time Series Models
    #' @param data The dataframe containing the time series realizations (data should not contain time index)
    #' @param var_interest The output variable of interest (dependent variable)
    #' @param mdl_list A names list of all models (see format below)
    #' @param n.ahead The number of observations used to calculate ASE or forecast ahead
    #' @param batch_size If any of the models used sliding ase method,
    #'                   then this number indicates the batch size to use
    #' @param step_n.ahead If using sliding window, should batches be incremented by n.ahead
    #'                     (Default = TRUE)
    #' @param verbose How much to print during the model building and other processes (Default = 0)
    #' @return A new `ModelCompareMultivariateVAR` object.
    initialize = function(data = NA, var_interest = NA, mdl_list, n.ahead = NA, batch_size = NA, step_n.ahead = TRUE, verbose = 0)
    {
      
      private$set_var_interest(var_interest = var_interest)
      super$initialize(data = data, mdl_list = mdl_list,
                       n.ahead = n.ahead, batch_size = batch_size, step_n.ahead = step_n.ahead,
                       verbose = verbose)
      
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the dependent variable name
    #' @return The dependent variable name
    get_var_interest = function(){return(private$var_interest)},
    
    #' @description Returns the dependent variable data only
    #' @return The dependent variable data only
    get_data_var_interest = function(){return(self$get_data()[, self$get_var_interest()])},
    
    #### General Public Methods ----
   
    #' @description Returns the AIC and the BIC for the model using the entire dataset
    #' @param sort_by 'AIC' or 'BIC'. Selects which column to sort the results by (Default: 'AIC')
    get_xIC = function(sort_by = "AIC"){
      results = dplyr::tribble(~Model, ~AIC, ~BIC)
      
      for (name in names(private$get_models())){
        AIC = private$models[[name]][['AIC']]
        BIC = private$models[[name]][['BIC']]
        
        results = results %>% 
          dplyr::add_row(Model = name, AIC = AIC, BIC = BIC)
        
      }  
      
      results = results %>% 
        dplyr::arrange_at(sort_by)
      
      return(results)
      
    }
    
  ),
  
  
  #### Private Methods ----
  private = list(
    var_interest = NA,
    
    set_var_interest = function(var_interest){private$var_interest = var_interest},
    
    get_len_x = function(){
      return(nrow(self$get_data()))
    },
    
    clean_model_input = function(mdl_list){
      
      mdl_list = super$clean_model_input(mdl_list)
      
      return(mdl_list)
    },
    
    get_sliding_ase_results = function(name, step_n.ahead){
      res = sliding_ase_var(data = self$get_data(),
                            var_interest = self$get_var_interest(),
                            k = private$get_models()[[name]][['k']],
                            trend_type = private$get_models()[[name]][['trend_type']],
                            n.ahead = self$get_n.ahead(),
                            batch_size = private$get_models()[[name]][['batch_size']],
                            step_n.ahead = step_n.ahead)
      
      return (res)
    },
    
    compute_simple_forecasts = function(lastn){
      
      results = dplyr::tribble(~Model, ~Time, ~f, ~ll, ~ul)
      
      if (lastn == FALSE){
        data_start = 1
        data_end = private$get_len_x()
        train_data = self$get_data()[data_start:data_end, ]
        
      }
      else{
        data_start = 1
        data_end = private$get_len_x() - self$get_n.ahead()
        train_data = self$get_data()[data_start:data_end, ]
      }
      
      from = data_end + 1
      to = data_end + self$get_n.ahead()
      
      # Define Train Data
      
      for (name in names(private$get_models())){
        
        var_interest = self$get_var_interest()
        k = private$get_models()[[name]][['k']]
        trend_type = private$get_models()[[name]][['trend_type']]
        
        # Fit model for the batch
        varfit = vars::VAR(train_data, p=k, type=trend_type)
        
        # Forecast for the batch
        forecasts = stats::predict(varfit, n.ahead=self$get_n.ahead())
        forecasts = forecasts$fcst[[var_interest]] ## Get the forecasts only for the dependent variable
        
        results = results %>% 
          dplyr::add_row(Model = name,
                         Time = (from:to),
                         f = forecasts[, 'fcst'],
                         ll = forecasts[, 'lower'],
                         ul = forecasts[, 'upper'])
        
      }
      
      return(results)
    },
    
    build_models  = function(verbose = 0){
      for (name in names(private$get_models())){
        cat("\n\n\n")
        cat(paste("Model: ", name))
        trend_type = private$get_models()[[name]][['trend_type']]
        cat(paste("\nTrend type: ", trend_type))
        
        varselect = vars::VARselect(self$get_data(), lag.max = private$get_models()[[name]][['lag.max']], type = trend_type, season = NULL, exogen = NULL)
        if (verbose >= 1){
          print(varselect) 
        }
        
        select = tolower(private$get_models()[[name]][['select']])
        if (select == 'aic'){
          k = varselect$selection[["AIC(n)"]] 
        }
        else if (select == 'bic'){
          k = varselect$selection[["SC(n)"]]  
        }
        else{
          stop("'select' argument must be with 'aic' or 'bic'")
        }
        print(paste("lag K to use for the VAR Model: ", k))  
        
        # Fit to Entire Data
        # This might be needed in many places to computing it here.
        varfit = vars::VAR(self$get_data(), p=k, type=trend_type)
        
        # a = summary(varfit)
        # 
        # print("build_models.1")
        # print(a$varresult[self$get_var_interest()])  # $coefficients[,"Pr(>|t|)"]
        
        ## Inplace
        private$models[[name]][['varselect_alldata']] = varselect
        private$models[[name]][['k']] = k
        private$models[[name]][['varfit_alldata']] = varfit
        
      }
    },
    
    evaluate_xIC = function(){
      for (name in names(private$get_models())){
        
        varfit = private$models[[name]][['varfit_alldata']]
        
        AIC = stats::AIC(varfit)
        BIC = stats::BIC(varfit)
        
        private$models[[name]][['AIC']] = AIC
        private$models[[name]][['BIC']] = BIC
      }  
    }
    
    
    
  )
  
)

