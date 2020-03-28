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
      
    },
    
    #' @description Returns the VAR model Build Summary
    #' @returns A dataframe containing the following columns
    #'          'Model': Name of the model
    #'          'Selection': The selection criteria used for K value (AIC or BIC)
    #'          'Trend': The trend argument used in the VARselect and VAR functions
    #'          'SlidingASE': Whether Sliding ASE will be used for this model
    #'          'Init_K': The K value recommended by the VARselect function
    #'          'Final_K': The adjusted K value to take into account the smaller batch size (only when using sliding_ase)
    summarize_build = function(){
      results = dplyr::tribble(~Model, ~Selection, ~Trend, ~SlidingASE, ~Init_K, ~Final_K)
      
      for (name in names(private$get_models())){
        results = results %>% 
          dplyr::add_row(Model = name,
                         Selection = private$models[[name]][['select']],
                         Trend = private$models[[name]][['trend_type']],
                         SlidingASE = private$models[[name]][['sliding_ase']],
                         Init_K = private$models[[name]][['k_initial']],
                         Final_K = private$models[[name]][['k_final']]
                         )
       
      }
      
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
                            k = private$get_models()[[name]][['k_final']],
                            trend_type = private$get_models()[[name]][['trend_type']],
                            n.ahead = self$get_n.ahead(),
                            batch_size = private$get_models()[[name]][['batch_size']],
                            step_n.ahead = step_n.ahead, verbose = private$get_verbose())
      
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
        k = private$get_models()[[name]][['k_final']]
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
        cat(paste("Model: ", name, "\n"))
        trend_type = private$get_models()[[name]][['trend_type']]
        cat(paste("Trend type: ", trend_type, "\n"))
        
        varselect = vars::VARselect(self$get_data(),
                                    lag.max = private$get_models()[[name]][['lag.max']],
                                    type = trend_type,
                                    season = NULL, exogen = NULL)
        
        if (verbose >= 1){
          cat("\nVARselect Object:\n")
          print(varselect) 
        }
        
        selection = private$extract_correct_varselection(varselect)
        
        select = tolower(private$get_models()[[name]][['select']])
        if (select == 'aic'){
          k = selection[["AIC(n)"]]
        }
        else if (select == 'bic'){
          k = selection[["SC(n)"]]
        }
        else{
          stop("'select' argument must be with 'aic' or 'bic'")
        }
        cat(paste("Lag K to use for the VAR Model: ", k, "\n")) 
        
        k_final = k
        ## If using sliding ASE, make sure that the batch size is large enough to support the number of lags
        if (private$get_models()[[name]][['sliding_ase']] == TRUE){
          k_final = private$validate_k(k)
        }
        
        # Fit to Entire Data
        # This might be needed in many places to computing it here.
        varfit = vars::VAR(self$get_data(), p=k_final, type=trend_type)
        
        if (verbose >= 1){
          cat(paste0("\n\nPrinting summary of the VAR fit for the variable of interest: ", var_interest, "\n"))
          print(summary(varfit$varresult[[var_interest]]))
        }
        
        ## Inplace
        private$models[[name]][['varselect_alldata']] = varselect
        private$models[[name]][['k_initial']] = k
        private$models[[name]][['k_final']] = k_final
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
    },
    
    extract_correct_varselection = function(vselect){
      criteria = vselect$criteria
      
      if(sum(criteria == -Inf, na.rm = TRUE) > 0){
        warning("VARselect produced -Inf values. These will be removed before making final 'K' selection.")
        
        criteria[criteria == -Inf] = max(criteria) + 1
      }
      
      selection = data.frame(t(Rfast::rowMins(criteria)))
      colnames(selection) = rownames(criteria)
      
      selection = selection %>% 
        assertr::verify(assertr::has_all_names("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)"))
      
      return(selection)
    },
    
    validate_k = function(k){
      new_k = k
      num_vars = ncol(self$get_data())
      if (k * num_vars >= (self$get_batch_size()-1)){
        new_k = floor((self$get_batch_size()-1)/num_vars) - 1 # Being a little more conservative 
        warning("Although the lag value k: ", k, " selected by VARselect will work for your full dataset, is too large for your batch size. Reducing k to allow Batch ASE calculations. New k: ", new_k, " If you do not want to reduce the k value, increase the batch size or make sliding_ase = FALSE for this model in the model list")
      }
      return((new_k))
    }
    
    
    
    
    
  )
  
)

