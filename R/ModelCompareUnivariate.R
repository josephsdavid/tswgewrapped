#' @title R6 class ModelCompareUnivariate
#' 
#' @examples 
#' library(tswge)
#' data("airlog")
#' 
#' # Woodward Gray Airline Model
#' phi_wg = c(-0.36, -0.05, -0.14, -0.11, 0.04, 0.09, -0.02, 0.02, 0.17, 0.03, -0.10, -0.38)
#' d_wg = 1
#' s_wg = 12
#' 
#' # Parzen Model
#' phi_pz = c(0.74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.38, -0.2812)
#' s_pz = 12
#' 
#' # Box Model
#' d_bx = 1
#' s_bx = 12  
#' theta_bx =  c(0.40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.60, -0.24)
#' 
#' models = list("Woodward Gray Model A" = list(phi = phi_wg, d = d_wg, s = s_wg, sliding_ase = FALSE),
#'               "Woodward Gray Model B" = list(phi = phi_wg, d = d_wg, s = s_wg, sliding_ase = TRUE),
#'               "Parzen Model A" = list(phi = phi_pz, s = s_pz, sliding_ase = FALSE),
#'               "Parzen Model B" = list(phi = phi_pz, s = s_pz, sliding_ase = TRUE),
#'               "Box Model A" = list(theta = theta_bx, d = d_bx, s = s_bx, sliding_ase = FALSE),
#'               "Box Model B" = list(theta = theta_bx, d = d_bx, s = s_bx, sliding_ase = TRUE)
#'               )
#'               
#' mdl_compare = ModelCompareUnivariate$new(data = airlog, mdl_list = models,
#'                                          n.ahead = 36, batch_size = 72)
#' # Plots the historgam of the ASE values for each model.
#' # This is especially useful when models using a sliding window for ASE calculations. 
#' mdl_compare$plot_boxplot_ases()
#' 
#' # The following method gives 2 plots
#' # (1) Plots the forecasts for each model along with the realization.
#' # (2) Plots the upper and lower limits for each model along with the realization.
#' # In both cases, this marks each batch using a background color for ease of comparison.
#' # only_sliding = TRUE will only plot forecsts for models using sliding ASE  calculations.
#' mdl_compare$plot_batch_forecasts(only_sliding = TRUE)
#' 
#' # This method statistically compares all the models that use a sliding window ASE calculation
#' mdl_compare$statistical_compare()  
#' 
#' ASEs = mdl_compare$get_tabular_metrics(ases = TRUE)
#' print(ASEs)
#'
#' # This method returns the metrics (ASE values) or forecasts for each model
#' # 'only_sliding' If set to TRUE, only the models that use a sliding window 
#' #                ASE calculation will be returned
#' # 'ases' If set to TRUE, this method will return the ASE value(s) 
#' #        Single value for models that don't use sliding ASEs and 
#' #        Multiple values (one per batch) for models that use sliding window
#' #        ASE calculations
#' #        If set to FALSE, this function will return the model forecasts and 
#' #        upper and lower confidence intervals. 
#' forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
#' print(forecasts)
#' 
#' @export
ModelCompareUnivariate = R6::R6Class(
  classname = "ModelCompareUnivariate",
  inherit = ModelCompareBase,
  cloneable = TRUE,
  lock_objects=F,
  lock_class=F,
  
  #### Public Methods ----
  public=list(
    #### Constructor ----
    
    #' @description 
    #' Initialize an object to compare several Univatiate Time Series Models
    #' @param data A Univariate Time Series Realization
    #' @param var_interest If data is a dataframe with multiple columns, then what is the output variable of interest
    #' @param mdl_list A named list of all models (see format below)
    #' @param n.ahead The number of observations used to calculate ASE or forecast ahead
    #' @param batch_size If any of the models used sliding ase method,
    #'                   then this number indicates the batch size to use
    #' @param step_n.ahead If using sliding window, should batches be incremented by n.ahead
    #'                     (Default = TRUE)
    #' @param verbose How much to print during the model building and other processes (Default = 0)
    #' @return A new `ModelCompareUnivariate` object.
    initialize = function(data = NA, var_interest = NA,  mdl_list, n.ahead = NA, batch_size = NA, step_n.ahead = TRUE, verbose = 0)
    {
      if ("data.frame" %in% class(data) | "matrix" %in% class(data)){
        if (ncol(data) == 1){
          ## Only 1 column, so just pick it
          data = data[,1]
        }
        else{
          if (is.na(var_interest)){
            stop("You have provided multiple columns in the data, but have not specified the column that you want to use for univariate modeling. Please provide this through the 'var_interest' argument.")
          }
          data = as.data.frame(data) %>% 
            assertr::verify(assertr::has_all_names(var_interest)) %>% 
            purrr::pluck(var_interest)
        }
      }
      
      super$initialize(data = data, mdl_list = mdl_list,
                       n.ahead = n.ahead, batch_size = batch_size, step_n.ahead = step_n.ahead,
                       verbose = verbose)
      
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the dependent variable data only
    #' @return The dependent variable data only
    get_data_var_interest = function(){return(self$get_data())},
    
    #### General Public Methods ----
    
    #' @description Creates multiple realization of each model. Useful to check model appropriateness.
    #' @param n.realizations Number of realization to create (Default: 4)
    #' @param lag.max lag.max to plot for ACF (Default: 25)
    #' @param seed The seed to use for generating realizations
    #' @param plot A vector of options to plot (Default = c("all"))
    #'             Other options: 'realization', 'acf', 'spectrum' 
    #' @param scales The scales argument to be passed to ggplot facet_wrap layer
    #'               (Default = 'free_y') Other appropriate options: 'fixed'
    plot_multiple_realizations = function(n.realizations = 4, lag.max = 25, seed = NA, plot = c("all"), scales = 'free_y'){
      final.data = NA
      final.results = NA
      
      for (name in names(private$get_models())){
        r = generate_multiple_realization(x = self$get_data(),
                                          phi = private$models[[name]][['phi']],
                                          theta = private$models[[name]][['theta']],
                                          d = private$models[[name]][['d']],
                                          s = private$models[[name]][['s']],
                                          vara = private$models[[name]][['vara']],
                                          n.realizations = n.realizations, lag.max = lag.max, seed = seed,
                                          model_name = name, return = 'all')
      
        if (all(is.na(final.data))){
          final.data = r$data
        }
        else{
          final.data = rbind(final.data, r$data)
        }
        
        if (all(is.na(final.results))){
          final.results = r$results
        }
        else{
          final.results = rbind(final.results, r$results)
        }
          
       
      }
      
      plot_multiple_realizations(data = final.data, results = final.results, plot = plot, scales = scales)
      
    },
    
    #' @description For the models for which the residuals have been provided,
    #' this method will check whetehr the residuals are white noise or not.
    #' (1) Plots the residuals and the ACF values
    #' (2) Performs the Ljung-Box test for K = 24 and K = 48
    #' @param lag.max The maximum lag to plot for the ACF
    #' @return A dataframe containing the results of the 2 Ljung-Box tests
    evaluate_residuals = function(lag.max = 50){
      final.results = NA
      any_present = 0
      
      for (name in names(private$get_models())){
        if (!is.null(private$models[[name]][['res']])){
          any_present = 1
          cat(paste("\n\nEvaluating residuals for model: '", name, "' \n", sep = ""))
          
          if (length(private$models[[name]][['phi']]) == 1){
            if (private$models[[name]][['phi']] == 0){
              p = 0  
            }
            else{
              p = length(private$models[[name]][['phi']])
            }
          }
          else{
            p = length(private$models[[name]][['phi']])
          }
          
          if (length(private$models[[name]][['theta']]) == 1){
            if (private$models[[name]][['theta']] == 0){
              q = 0  
            }
            else{
              q = length(private$models[[name]][['theta']])
            }
          }
          else{
            q = length(private$models[[name]][['theta']])
          }
          
          table = evaluate_residuals(private$models[[name]][['res']], p = p, q = q, model_name = name, lag.max = lag.max)
        
          if (all(is.na(final.results))){
            final.results = table
          }
          else
            final.results = rbind(final.results, table)
        }
      }
      
      if (any_present == 0){
        cat("\n\nNone of the model that you supplied had any residuals provided. Hence the residuals can not be evaluated")
      }
      
      return(final.results)
    },
    
    #' @description Not applicable for the Univariate Compare, since we are passing already build models
    summarize_build = function(){

    }
    
    
  ),
  
  
  #### Private Methods ----
  private = list(
    get_len_x = function(){
      return(length(self$get_data()))
    },
    
    clean_model_input = function(mdl_list, batch_size){
      # If the inputs are missing p, d, q, or s values, this will add 0s to make it consistent
      for (name in names(mdl_list)){
        if (is.null(mdl_list[[name]][['phi']])){
          mdl_list[[name]][['phi']] = 0
        }
        if (is.null(mdl_list[[name]][['d']])){
          mdl_list[[name]][['d']] = 0
        }
        if (is.null(mdl_list[[name]][['theta']])){
          mdl_list[[name]][['theta']] = 0
        }
        if (is.null(mdl_list[[name]][['s']])){
          mdl_list[[name]][['s']] = 0
        }
        
      }
      
      mdl_list = super$clean_model_input(mdl_list)
      
      return(mdl_list)
    },
    
    get_sliding_ase_results = function(name, step_n.ahead){
      res = sliding_ase_univariate(x = self$get_data(),
                                   phi = private$get_models()[[name]][['phi']],
                                   theta = private$get_models()[[name]][['theta']],
                                   d = private$get_models()[[name]][['d']],
                                   s = private$get_models()[[name]][['s']],
                                   n.ahead = self$get_n.ahead(),
                                   batch_size = private$get_models()[[name]][['batch_size']],
                                   step_n.ahead = step_n.ahead, verbose = private$get_verbose())
      
      return (res)
    },
    
    compute_simple_forecasts = function(lastn, newxreg){
      ## newxreg is not needed for Univariate Analysis
      
      results = dplyr::tribble(~Model, ~Time, ~f, ~ll, ~ul)
      
      if (lastn == FALSE){
        from = private$get_len_x() + 1
        to = private$get_len_x() + self$get_n.ahead()
      }
      else{
        from = private$get_len_x() - self$get_n.ahead() + 1
        to = private$get_len_x()
      }
      
      for (name in names(private$get_models())){
        forecast = tswge::fore.aruma.wge(x = self$get_data(),
                                         phi = private$get_models()[[name]][['phi']],
                                         theta = private$get_models()[[name]][['theta']],
                                         d = private$get_models()[[name]][['d']],
                                         s = private$get_models()[[name]][['s']], 
                                         n.ahead = self$get_n.ahead(),
                                         lastn = lastn, plot = FALSE)
        
        results = results %>%  dplyr::add_row(Model = name,
                                              Time = (from:to),
                                              f = forecast$f,
                                              ll = forecast$ll,
                                              ul = forecast$ul
        )
        
      }
      
      return(results)
    },
    
    set_var_interest = function(var_interest = NA){
      ## Do nothing. There is only 1 variable of interest.
    }
    

    
  )
  
)

