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
    #' @param alpha Significance level to use for filtering of variables from the recommendations (Default = 0.05)
    #' @param verbose How much to print during the model building and other processes (Default = 0)
    #' @param ... Additional parameers to feed to VARSelect (if applicable) and VAR --> Most notably "exogen"
    #' @return A new `ModelCompareMultivariateVAR` object.
    initialize = function(data = NA, var_interest = NA, mdl_list, alpha = 0.05, verbose = 0, ...)
    {
      
      self$set_verbose(verbose = verbose)
      self$set_alpha(alpha = alpha)
      private$set_data(data = data)
      private$set_var_interest(var_interest = var_interest)
      
      self$add_models(mdl_list, alpha = private$get_alpha(), ...)
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
    
    #' @description Set the significance level to use for filtering of variables from the recommendations
    #' @param alpha Significance level to use (Default = 0.05)
    set_alpha = function(alpha = 0.05){
      private$alpha = alpha
    },
    
    #### General Public Methods ----
   
    #' @description Returns the VAR model Build Summary
    #' @returns A dataframe containing the following columns
    #'          'Model': Name of the model
    #'          'Selection': The selection criteria used for K value (AIC or BIC)
    #'          'Trend': The trend argument used in the VARselect and VAR functions
    #'          'SlidingASE': Whether Sliding ASE will be used for this model
    #'          'Init_K': The K value recommended by the VARselect function
    #'          'Final_K': The adjusted K value to take into account the smaller batch size (only when using sliding_ase)
    summarize_build = function(){
      results = dplyr::tribble(~Model, ~select, ~trend_type, ~season, ~p, ~SigVar, ~OriginalVar, ~Lag, ~MaxLag)
      
      for (name in names(private$get_models())){
        results = results %>% 
          dplyr::add_row(Model = name,
                         select = private$models[[name]][['select']],
                         trend_type = private$models[[name]][['trend_type']],
                         season = ifelse(is.null(private$models[[name]][['season']]), 0, private$models[[name]][['season']]),
                         p = private$models[[name]][['p']],
                         SigVar = private$models[[name]][['sigvars']][['sig_var']],
                         OriginalVar = private$models[[name]][['sigvars']][['original_var']],
                         Lag = private$models[[name]][['sigvars']][['lag']],
                         MaxLag = private$models[[name]][['sigvars']][['max_lag']]
                         )
      }
      
      return(results)
    },
    
    #' @description Returns a dataframe with recommended variables to use 
    #' for each VAR model along with its corresponding lag value
    #' @return A data frame with the recommendations
    #' (1) Number of significant variables
    #' (2) The names of the significant variables to use
    #' (3) Lag value to use for the model
    get_recommendations = function(){
      
      # Check if any seasonality term is significant
      season_data = self$summarize_build() %>% 
        dplyr::group_by(Model, trend_type, season) %>% 
        dplyr::filter(grepl("^sd[0-9]+$", OriginalVar)) %>% 
        dplyr::summarise(num_sig_season_factors = n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(sig_season_factors = ifelse(num_sig_season_factors >= 1, TRUE, FALSE)) %>% 
        dplyr::select(-num_sig_season_factors)
      
      results = self$summarize_build() %>% 
        dplyr::filter(!(OriginalVar %in% c("trend", "const"))) %>% 
        dplyr::filter(!(grepl("^sd[0-9]+$", OriginalVar))) %>% # Remove seasonality factors
        dplyr::group_by(Model, trend_type, season) %>% 
        dplyr::summarise(num_sig_lag_vars = dplyr::n_distinct(OriginalVar),
                         lag_to_use = -min(MaxLag, na.rm = TRUE),
                         lag_vars_to_use =  paste0(unique(OriginalVar), collapse = ",")) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(season_data, by = c("Model", "trend_type", "season")) %>% 
        dplyr::mutate(season_to_use = ifelse(sig_season_factors == TRUE, season, 0)) %>% 
        tidyr::replace_na(list(season_to_use = 0)) %>% 
        dplyr::select(-sig_season_factors)
      
      return(results)
    },
    
    #' @description Builds the models with the recommended lags and variables
    build_recommended_models = function(){
      recommendations = self$get_recommendations() %>% 
        dplyr::mutate(Model = paste0(Model, " - R")) %>% 
        dplyr::mutate(model_built = FALSE) %>% 
        tibble::column_to_rownames(var = "Model") %>% 
        dplyr::rename(p = lag_to_use) %>% 
        dplyr::select(-num_sig_lag_vars) 
      
      # https://stackoverflow.com/questions/3492379/data-frame-rows-to-a-list
      recommendations = setNames(split(recommendations, seq(nrow(recommendations))), rownames(recommendations))
      
      for (name in names(recommendations)){
        if (recommendations[[name]][['model_built']] == FALSE){
          
          trend_type = recommendations[[name]][['trend_type']]
          season = recommendations[[name]][['season_to_use']]
          if (season == 0){
            season = NULL
          }
          
          if (private$get_verbose() >= 1){
            cat(paste0("\nModel: ", name))
            cat(paste0("\nTrend type: ", trend_type))
            cat(paste0("\nSeasonality: ", season))
          }
          
          col_names = unlist(strsplit(recommendations[[name]][['lag_vars_to_use']], split = ","))
          col_names = c(self$get_var_interest(), col_names)
          col_names = unique(col_names)

          if (length(col_names) == 1){
            warning("This recommendation is to use just the variable of interest (dependent variable) to model the time series, hence this model will not be built. Please use a univariate approach to model this separately.")
          }
          else{
            selected_data = private$get_selected_data(col_names)

            # Fit to Entire Data
            # This might be needed in many places to computing it here.
            varfit = vars::VAR(selected_data,
                               p = recommendations[[name]][['p']],
                               type = trend_type,
                               season = season #,
                               #...
                               )

            ## Find the significant variables only
            results = summary(varfit[['varresult']][[self$get_var_interest()]])

            if (private$get_verbose() >= 1){
              cat(paste0("\n\nPrinting summary of the VAR fit for the variable of interest: ", self$get_var_interest()))
              print(results)
            }

            ## Inplace
            # https://stackoverflow.com/questions/32626925/problems-using-double-lists
            # If the first element inserted is not a list, then if we subsequently try to add a list, it gives error
            # Hence kept the varfit first
            private$models[[name]][['varfit']] = varfit  ## TODO: Check Need to add list here else it gives error. 
            private$models[[name]][['p']] = recommendations[[name]][['p']]
            private$models[[name]][['sigvars']] = NA  # We dont care here since we are going to use all
            private$models[[name]][['model_type']] = "recommended"
            private$models[[name]][['model_built']] = TRUE
          }
        }
        else{
          warning(paste("Model: '", name, "' has already been built. This will not be built again."))
        }
      }
      
    },
    
    #' @description Returns a final models 
    #' @param subset The subset of models to get.
    #'              'a': All models (Default)
    #'              'u': Only User Defined Models
    #'              'r': Only the recommended models
    #' @param mdl_names Vector of model names to get. This honors the subset variable.
    #' @return A named list of models
    get_final_models = function(subset = 'a', mdl_names = NA){
      if (subset != 'a' & subset != 'u' & subset != 'r'){
        warning("The subset value mentioned is not correct. Allowed values are 'a', 'u' or 'r. The default value 'a' will be used")
        subset = 'a'
      }
      
      mdl_subset_names = c()

      for (name in names(private$get_models())){
        if (subset == 'a' | subset == 'r'){
          if (private$models[[name]][['model_type']] == 'recommended'){
            if (is.na(mdl_names)){
              mdl_subset_names = c(mdl_subset_names, name)  
            }
            else{
              if (name %in% mdl_names){
                mdl_subset_names = c(mdl_subset_names, name)
              }
            }
          }
        }
        if (subset == 'a' | subset == 'u'){
          if (private$models[[name]][['model_type']] == 'user_defined'){
            if (is.na(mdl_names)){
              mdl_subset_names = c(mdl_subset_names, name)  
            }
            else{
              if (name %in% mdl_names){
                mdl_subset_names = c(mdl_subset_names, name)
              }
            }  
          }
        }
      }
      
      mdl_subset = list()
      
      for (name in mdl_subset_names){
        mdl_subset[[name]][['varfit']] = private$models[[name]][['varfit']]
        mdl_subset[[name]][['sliding_ase']] = FALSE
      }
      
      return(mdl_subset)
    },
    
    #' @description Add models to the existing object
    #' @param mdl_list The list of new models to add
    #' @param alpha Significance level to use for filtering of variables from the recommendations (Default = 0.05)
    #' @param ... Additional parameers to feed to VARSelect (if applicable) and VAR --> Most notably "exogen"
    add_models = function(mdl_list, alpha = NA, ...){
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
      
      if (is.na(alpha)){
        alpha = private$get_alpha()
      }
      
      private$build_models(verbose = private$get_verbose(), alpha = alpha, ...)
      
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
    alpha = NA,
    verbose = NA,
    
    set_data = function(data){
      if (all(is.na(data))){ stop("You have not provided the time series data. Please provide to continue.") }
      private$data = data
    },
    
    get_selected_data = function(col_names){
      return(self$get_data() %>% dplyr::select(col_names))
    },
    
    set_var_interest = function(var_interest){private$var_interest = var_interest},
    
    get_models = function(){
      return(private$models)
    },
    
    get_alpha = function(){
      return(private$alpha)
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
    
    build_models  = function(verbose = 0, alpha, ...){
      for (name in names(private$get_models())){
        if (private$get_models()[[name]][['model_built']] == FALSE){
          
          trend_type = private$get_models()[[name]][['trend_type']]
          season = private$get_models()[[name]][['season']]
          
          if (private$get_verbose() >= 1){
            cat(paste0("\nModel: ", name))
            cat(paste0("\nTrend type: ", trend_type))
            cat(paste0("\nSeasonality: ", season))
          }
          
          varselect = vars::VARselect(self$get_data(),
                                      lag.max = private$get_models()[[name]][['lag.max']],
                                      type = trend_type,
                                      season = season,
                                      ...)
          
          if (private$get_verbose() >= 1){
            cat("\nVARselect Object:\n")
            print(varselect) 
          }
          
          selection = private$extract_correct_varselection(varselect)
          
          select = tolower(private$get_models()[[name]][['select']])
          if (select == 'aic'){
            p = selection[["AIC(n)"]]
          }
          else if (select == 'bic'){
            p = selection[["SC(n)"]]
          }
          else{
            stop("'select' argument must be with 'aic' or 'bic'")
          }
          
          if (private$get_verbose() >= 1){
            cat(paste("\nLag K to use for the VAR Model: ", p)) 
          }
          
          # Fit to Entire Data
          # This might be needed in many places to computing it here.
          varfit = vars::VAR(self$get_data(),
                             p = p,
                             type = trend_type,
                             season = season,
                             ...
                             )
          
          ## Find the significant variables only
          results = summary(varfit[['varresult']][[self$get_var_interest()]])
          
          if (private$get_verbose() >= 1){
            cat(paste0("\nPrinting summary of the VAR fit for the variable of interest: ", self$get_var_interest(), "\n"))
            print(results)
          }
          
          ## Inplace
          # https://stackoverflow.com/questions/32626925/problems-using-double-lists
          # If the first element inserted is not a list, then if we subsequently try to add a list, it gives error
          # Hence kept the varfit first
          private$models[[name]][['varselect']] = varselect
          private$models[[name]][['varfit']] = varfit
          private$models[[name]][['p']] = p
          private$models[[name]][['sigvars']] = private$get_significant_vars(results, alpha)  # What were the significant columns (variables in the model)
          private$models[[name]][['model_type']] = "user_defined"
          private$models[[name]][['model_built']] = TRUE
          
        }
        else{
          warning(paste("Model: '", name, "' has already been built. This will not be built again."))
        }
        
      }
    },
    
    extract_correct_varselection = function(vselect){
      criteria = vselect$criteria
      
      if(sum(criteria == -Inf, na.rm = TRUE) > 0){
        cat("\n")
        warning("VARselect produced -Inf values. These will be removed before making final 'K' selection.")
        cat("\n")
        
        criteria[criteria == -Inf] = max(criteria) + 1
      }
      
      selection = data.frame(t(Rfast::rowMins(criteria)))
      colnames(selection) = rownames(criteria)
      
      selection = selection %>% 
        assertr::verify(assertr::has_all_names("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)"))
      
      return(selection)
    },
    
    get_significant_vars = function(results, alpha){
      significant = as.data.frame(results[["coefficients"]])[which(results[["coefficients"]][,"Pr(>|t|)"] < alpha), ]
      sig_vars = as.data.frame(rownames(significant))
      colnames(sig_vars) = "sig_var"
      
      sig_vars = sig_vars %>% 
        tidyr::separate(col = sig_var, into = c("original_var", "lag"), sep = "\\.", remove = FALSE) %>% 
        tidyr::replace_na(list(lag = 0)) %>% 
        dplyr::mutate(lag = -as.numeric(stringr::str_replace(lag, "l", ""))) %>% 
        dplyr::group_by(original_var) %>% 
        dplyr::mutate(max_lag = min(lag)) %>% 
        dplyr::ungroup() 
      
      return(sig_vars)
    }
    
    
    
    
    
    
    
  )
  
)

