#' @title R6 class MultivariateEDA
#' 
#' @export
MultivariateEDA = R6::R6Class(
  classname = "MultivariateEDA",
  cloneable = TRUE,
  lock_objects=F,
  lock_class=F,
  
  #### Public Methods ----
  public=list(
    #### Constructor ----
    
    #' @description 
    #' Initialize an object to perform EDA on a Multivariate Time Series 
    #' @param data The dataframe (or type that can be coerced to a dataframe) containing the time series realizations 
    #' @param var_interest The output variable of interest (dependent variable)
    #' @param var_time If the dataframe has a time column, what is the name of this column?
    #' @param verbose How much to print during the model building and other processes (Default = 0)
    #' @return A new `MultivariateEDA` object.
    initialize = function(data = NA, var_interest = NA, var_time = NA, verbose = 0)
    {
      private$set_data(data = data)
      private$set_var_interest(var_interest = var_interest)
      private$set_var_time(var_time = var_time)
      self$set_verbose(verbose = verbose)
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the time series realization
    #' @param time NA will return the original data without the 'var_time' column
    #'             'original' will return the original data with any 'var_time' column (if applicable)
    #'             'sub' will return the original data without any 'var_time' column (if applicable)
    #'                   but with a substitute 'Time' column which is equal to the observation number
    #' @return The Time Series Realization
    get_data = function(time = NA){
      if (is.na(time)){
        if (is.na(self$get_var_time())){
          rvData = private$data
        }
        else{
          rvData = private$data %>% 
            dplyr::select(-!!self$get_var_time())
        }
      }
      else if (time == 'original'){
        rvData = private$data
      }
      else if (time == 'sub'){
        if (is.na(self$get_var_time())){
          rvData = private$data %>% 
            dplyr::mutate(Time = dplyr::row_number())
        }
        else{
          rvData = private$data %>% 
            dplyr::select(-!!self$get_var_time()) %>% 
            dplyr::mutate(Time = dplyr::row_number())
        }
      }
      return(rvData)
    },
    
    #' @description Returns the dependent variable name
    #' @return The dependent variable name
    get_var_interest = function(){return(private$var_interest)},
    
    #' @description Returns the time  variable
    #' @return The time variable
    get_var_time = function(){return(private$var_time)},
    
    #' @description Returns the dependent variable data only
    #' @return The dependent variable data only
    get_data_var_interest = function(){
      rvData = self$get_data() %>% 
        dplyr::select(self$get_var_interest())
      return(rvData)
    },
    
    #' @description Adjust the verbosity level
    #' @param verbose 0 = Minimal Printing only (usualy limited to step being performed)
    #'                1 = Basic printing of model builds, etc.
    #'                2 = Reserved for debugging mode. May slow down the run due to excessive printing, especially when using batches   
    set_verbose = function(verbose = 0){
      private$verbose = verbose
    },
    
    #### General Public Methods ----
   
    #' @description Plots the time series with all the dependent variables
    #' @param ncol Number of columns to use to show the data
    #' @param scales The scales argument to be passed to ggplot facet_wrap layer
    #'               (Default = 'free_y') Other appropriate options: 'fixed'
    #' @param ... Arguments to pass to facet wrap. Example "ncol = 3, scales = 'free_y'"
    plot_data = function(ncol = 1, scales = 'free_y', ...){
      df_melted = self$get_data(time = 'sub') %>% 
        tidyr::gather(variable, value, -Time)
      
      p = ggplot2::ggplot(df_melted) + 
        ggplot2::facet_wrap(variable ~ ., ncol = ncol, scales = scales, ...) + 
        ggplot2::geom_line(mapping = ggplot2::aes(x = Time, y = value, colour = variable))
      
      print(p)
      
    },
    
    #' @description Plots the scatterplots matric of all the variables in the dta
    plot_scatterplots = function(){
      print(GGally::ggpairs(self$get_data(time = NA))) #matrix of scatter plots  
    },
    
    #' @description Plots the CCF function for the dependent variable against all independent variables
    #' @param lag.max The maximum lag to evaluate
    #' @param negative_only Whether to take max cross correlation of only negative lags for the independent variables.
    #'                      Many times durign predictions, we dont have future values available for the independent variables
    #'                      In such cases, we can not use positive lag values for predictions. (Default = TRUE)
    #' @return A dataframe containing 
    #'         (1) 'variable': the dependent variable name, 
    #'         (2) 'max_ccf_index': lag at which max cross correlation occurs
    #'         (3) 'max_ccf_value': max cross correlation value (abs)
    #'         (4) 'max_ccf_index_adjusted': adjusted index (if negative_only is FALSE, then this will show a value capped 
    #'              at lag = 0 for any positive lag index). User may then decide to use either this value or the 'max_ccf_index'
    #'              dependig on if the positive lag values of the dependent variable will be available for the prediction
    plot_ccf_analysis = function(lag.max = 12, negative_only = TRUE){
      
      results = dplyr::tribble(~variable, ~max_ccf_index, ~max_ccf_value)
      for (name in colnames(self$get_data(time = NA))){
        if (name != self$get_var_interest()){
          # By convention, the X2 variable comes first
          c = stats::ccf(self$get_data(time = NA)[name], self$get_data_var_interest(), lag.max = lag.max, plot = FALSE)
          
          subset_lag_acfs = c$acf[,,1]
          
          if (negative_only == TRUE){
            subset_lags = c$lag[,,1] <= 0
            subset_lag_acfs = c$acf[,,1][subset_lags]
          }
          
          index = which(abs(subset_lag_acfs) == max(abs(subset_lag_acfs)))
          max_ccf_index = c$lag[,1,1][index] 
          max_ccf_value = c$acf[,1,1][index]
          
          plot(c, main = paste("\nVariable: ", name, " , max cross-correlation @ at lag: ", max_ccf_index, sep = ""))
          
          results = results %>% 
            dplyr::add_row(variable = name, max_ccf_index = max_ccf_index, max_ccf_value = max_ccf_value)
        }
      }
      
      results = results %>% 
        dplyr::mutate(max_ccf_index_adjusted = ifelse(max_ccf_index > 0, 0, max_ccf_index)) %>% 
        dplyr::arrange(desc(abs(max_ccf_value)))  
      
      return(results)
    },
    
    plot_lag_plots = function(){
      
    }
   
  ),
  
  
  #### Private Methods ----
  private = list(
    data = NA,
    var_interest = NA,
    var_time = NA,
    verbose = NA,
    
    set_data = function(data){
      if (all(is.na(data))){ stop("You have not provided the time series data. Please provide to continue.") }
      private$data = as.data.frame(data)
    },
    
    set_var_interest = function(var_interest){private$var_interest = var_interest},
    
    set_var_time = function(var_time){private$var_time = var_time},
    
    get_verbose = function(){
      return(private$verbose)
    }
    
    
    
  )
  
)

