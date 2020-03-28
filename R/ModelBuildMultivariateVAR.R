#' @title R6 class ModelBuildMultivariateVAR
#' 
#' @export
ModelBuildMultivariateVAR = R6::R6Class(
  classname = "ModelBuildMultivariateVAR",
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
    #' @param verbose How much to print during the model building and other processes (Default = 0)
    #' @param ... Additional parameers to feed to VARSelect (if applicable) and VAR --> Most notably "exogen"
    #' @return A new `ModelCompareMultivariateVAR` object.
    initialize = function(data = NA, var_interest = NA, mdl_list, verbose = 0, ...)
    {
      
      self$set_verbose(verbose = verbose)
      private$set_data(data = data)
      private$set_var_interest(var_interest = var_interest)
      
      self$add_models(mdl_list, ...)
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the time series realization
    #' @return The Time Series Realization
    get_data = function(){return(private$data)},
    
    #' @description Returns the dependent variable name
    #' @return The dependent variable name
    get_var_interest = function(){return(private$var_interest)},
    
    #' @description Returns the dependent variable data only
    #' @return The dependent variable data only
    get_data_var_interest = function(){return(self$get_data()[, self$get_var_interest()])},
    
    #' @description Adjust the verbosity level
    #' @param verbose 0 = Minimal Printing only (usualy limited to step being performed)
    #'                1 = Basic printing of model builds, etc.
    #'                2 = Reserved for debugging mode. May slow down the run due to excessive printing, especially when using batches   
    set_verbose = function(verbose = 0){
      private$verbose = verbose
    },
    
    #### General Public Methods ----
   
    # #' @description Returns the AIC and the BIC for the model using the entire dataset
    # #' @param sort_by 'AIC' or 'BIC'. Selects which column to sort the results by (Default: 'AIC')
    # get_xIC = function(sort_by = "AIC"){
    #   results = dplyr::tribble(~Model, ~AIC, ~BIC)
    #   
    #   for (name in names(private$get_models())){
    #     AIC = private$models[[name]][['AIC']]
    #     BIC = private$models[[name]][['BIC']]
    #     
    #     results = results %>% 
    #       dplyr::add_row(Model = name, AIC = AIC, BIC = BIC)
    #     
    #   }  
    #   
    #   results = results %>% 
    #     dplyr::arrange_at(sort_by)
    #   
    #   return(results)
    #   
    # },
    
    #' @description Returns the VAR model Build Summary
    #' @returns A dataframe containing the following columns
    #'          'Model': Name of the model
    #'          'Selection': The selection criteria used for K value (AIC or BIC)
    #'          'Trend': The trend argument used in the VARselect and VAR functions
    #'          'SlidingASE': Whether Sliding ASE will be used for this model
    #'          'Init_K': The K value recommended by the VARselect function
    #'          'Final_K': The adjusted K value to take into account the smaller batch size (only when using sliding_ase)
    summarize_build = function(){
      results = dplyr::tribble(~Model, ~Selection, ~Trend, ~Season, ~K, ~SigVar, ~OriginalVar, ~Lag, ~MaxLag)
      
      for (name in names(private$get_models())){
        results = results %>% 
          dplyr::add_row(Model = name,
                         Selection = private$models[[name]][['select']],
                         Trend = private$models[[name]][['trend_type']],
                         Season = ifelse(is.null(private$models[[name]][['season']]), 0, private$models[[name]][['season']]),
                         K = private$models[[name]][['k']],
                         SigVar = private$models[[name]][['sigvars']][['sig_var']],
                         OriginalVar = private$models[[name]][['sigvars']][['original_var']],
                         Lag = private$models[[name]][['sigvars']][['lag']],
                         MaxLag = private$models[[name]][['sigvars']][['max_lag']]
                         )
      }
      
      return(results)
    },
    
    #' @description Add models to the existing object
    #' @param mdl_list The list of new models to add
    #' @param ... Additional parameers to feed to VARSelect (if applicable) and VAR --> Most notably "exogen"
    add_models = function(mdl_list, ...){
      if (length(unique(names(mdl_list))) != length(names(mdl_list))){
        stop("The model names in the provided list contain duplicates. Please fix and rerun.")
      }
      
      existing = names(private$models)
      if (any(names(mdl_list) %in% existing)){
        print(names(mdl_list)[names(mdl_list) %in% existing])
        stop("The model names above already exist in this comparison object. Please provide unique names.")
      }
      
      mdl_list = private$clean_model_input(mdl_list)
      
      if (all(is.na(private$models))){
        private$models = mdl_list
      }
      else{
        private$models = c(private$models, mdl_list)
      }
      
      private$build_models(verbose = private$get_verbose(), ...)
      
    },
    
    #' @description Remove models from the object
    #' @param mdl_names A vector of the model names to remove. 
    remove_models = function(mdl_names){
      for (name in mdl_names){
        if (name %in% names(private$get_models())){
          cat(paste0("\nModel: '", name, "' found in object. This will be removed."))
          private$models[[name]] = NULL
        }
        else{
          cat(paste0("\nModel: '", name, "' was not found in object. Please verify that the correct name."))
        }
      }
      cat("\n")
    },
    
    #' @description Keep only the provided models
    #' @param mdl_names A vector of the model names to keep. 
    keep_models = function(mdl_names){
      for (name in names(private$get_models())){
        if (!(name %in% mdl_names)){
          cat(paste0("\nModel: '", name, "' will be removed."))
          private$models[[name]] = NULL
        }
      }
      cat("\n")
    }
    
  ),
  
  
  #### Private Methods ----
  private = list(
    data = NA,
    var_interest = NA,
    models = NA,
    verbose = NA,
    
    set_data = function(data){
      if (all(is.na(data))){ stop("You have not provided the time series data. Please provide to continue.") }
      private$data = data
    },
    
    set_var_interest = function(var_interest){private$var_interest = var_interest},
    
    get_models = function(){
      return(private$models)
    },
    
    get_verbose = function(){
      return(private$verbose)
    },
    
    clean_model_input = function(mdl_list){
      # If the inputs are missing sliding_ase, make it FALSE
      # Also add 'model_built' key
      for (name in names(mdl_list)){
        if (is.null(mdl_list[[name]][['season']])){
          mdl_list[[name]][['season']] = NULL
        }
        if (is.null(mdl_list[[name]][['type']])){
          mdl_list[[name]][['type']] = "none"
        }
        
        mdl_list[[name]][['model_built']] = FALSE
        
      }
      
      return(mdl_list)
    },
    
    # get_len_x = function(){
    #   return(nrow(self$get_data()))
    # },

    build_models  = function(verbose = 0, ...){
      for (name in names(private$get_models())){
        cat("\n\n\n")
        cat(paste("Model: ", name, "\n"))
        trend_type = private$get_models()[[name]][['trend_type']]
        cat(paste("Trend type: ", trend_type, "\n"))
        
        varselect = vars::VARselect(self$get_data(),
                                    lag.max = private$get_models()[[name]][['lag.max']],
                                    type = trend_type,
                                    season = private$get_models()[[name]][['season']],
                                    ...)
        
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
        
        # Fit to Entire Data
        # This might be needed in many places to computing it here.
        varfit = vars::VAR(self$get_data(),
                           p = k,
                           type = trend_type,
                           season = private$get_models()[[name]][['season']],
                           ...
                           )
        
        ## Find the significant variables only
        results = summary(varfit[['varresult']][[self$get_var_interest()]])
        
        if (verbose >= 1){
          cat(paste0("\n\nPrinting summary of the VAR fit for the variable of interest: ", self$get_var_interest(), "\n"))
          print(results)
        }
        
        significant = as.data.frame(results$coefficients)[which(results$coefficients[,"Pr(>|t|)"] < 0.05), ]
        sig_vars = as.data.frame(rownames(significant))
        colnames(sig_vars) = "sig_var"
        
        sig_vars = sig_vars %>% 
          tidyr::separate(col = sig_var, into = c("original_var", "lag"), sep = "\\.", remove = FALSE) %>% 
          dplyr::mutate(lag = -as.numeric(stringr::str_replace(lag, "l", ""))) %>% 
          dplyr::group_by(original_var) %>% 
          dplyr::mutate(max_lag = min(lag)) %>% 
          dplyr::ungroup()
        
        ## Inplace
        private$models[[name]][['varselect']] = varselect
        private$models[[name]][['k']] = k
        private$models[[name]][['varfit']] = varfit
        private$models[[name]][['sigvars']] = sig_vars
        
      }
    },
    
    # evaluate_xIC = function(){
    #   for (name in names(private$get_models())){
    #     
    #     varfit = private$models[[name]][['varfit_alldata']]
    #     
    #     AIC = stats::AIC(varfit)
    #     BIC = stats::BIC(varfit)
    #     
    #     private$models[[name]][['AIC']] = AIC
    #     private$models[[name]][['BIC']] = BIC
    #   }  
    # },
    
    # evaluate_models = function(...){
    #   private$build_models(verbose = private$get_verbose(), ...)
    #   # private$evaluate_xIC()
    #   # self$compute_metrics(step_n.ahead = private$get_step_n.ahead())
    #   # print(self$summarize_build())  
    # },
    
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
    }
    
    
    
    
    
    
    
  )
  
)

