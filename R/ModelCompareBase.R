#' @title R6 class ModelCompareBase
ModelCompareBase = R6::R6Class(
  classname = "ModelCompareBase",
  cloneable = FALSE,
  lock_objects=F,
  lock_class=F,
  
  #### Public Methods ----
  public=list(
    #### Constructor ----
    
    #' @description 
    #' Initialize an object to compare several Univatiate Time Series Models
    #' @param data The dataframe containing the time series realizations (data should not contain time index)
    #' @param mdl_list A named list of all models (see format below)
    #' @param n.ahead The number of observations used to calculate ASE or forecast ahead
    #' @param batch_size If any of the models used sliding ase method,
    #'                   then this number indicates the batch size to use
    #' @param step_n.ahead If using sliding window, should batches be incremented by n.ahead
    #'                     (Default = TRUE)
    #' @param verbose How much to print during the model building and other processes (Default = 0)                      
    #' @return A new `ModelCompareBase` object.
    initialize = function(data = NA, mdl_list, n.ahead = NA, batch_size = NA, step_n.ahead = TRUE, verbose = 0)
    {
      self$set_verbose(verbose = verbose)
      private$set_data(data = data)
      private$set_n.ahead(n.ahead)
      private$set_step_n.ahead(step_n.ahead)
      
      self$add_models(mdl_list, batch_size)
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the time series realization
    #' @return The Time Series Realization
    get_data = function(){return(private$data)},
    
    #' @description Returns the dependent variable data only
    #' @return The dependent variable data only
    get_data_var_interest = function(){
      stop("You are calling the 'get_data_var_interest' method in the parent class. This should be implemented in the child class.")
    },
    
    #' @description Returns the batch size value
    #' @return The Batch Size Value
    get_batch_size = function(){return(private$batch_size)},
    
    #' @description Sets the batch size value
    #' @param batch_size Batch Size Value
    set_batch_size = function(batch_size){
      if (private$any_sliding_ase() & is.na(batch_size)){
        warning("You have provided models that require sliding ASE calculations, but the batch size has been set to NA. Setting the batch size to the length of the realization.")
        private$batch_size = private$get_len_x()
        
      }
      private$batch_size = batch_size
      private$set_batch_per_model()
      
      ## Evaluate models any time the batch size is set -- 
      ## (1) either when model are added (during initialization or manually) or 
      ## (2) when user resets batch size manually
      private$evaluate_models()
      
      
    },
    
    #' @description Returns the n.ahead value
    #' @return The n.ahead value
    get_n.ahead = function(){return(private$n.ahead)},
    
    #' @description Adjust the verbosity level
    #' @param verbose 0 = Minimal Printing only (usualy limited to step being performed)
    #'                1 = Basic printing of model builds, etc.
    #'                2 = Reserved for debugging mode. May slow down the run due to excessive printing, especially when using batches   
    set_verbose = function(verbose = 0){
      private$verbose = verbose
    },
    
    #' @description Get all the model names
    #' @param only_sliding If TRUE, this will only plot the ASEs for
    #'                     the models that used window ASE calculations
    #' @return A vector of all model names
    get_model_names = function(only_sliding = FALSE){
      
      model_subset = c()
      
      if (only_sliding == TRUE){
        for (name in names(private$get_models())){
          if (private$models[[name]][['sliding_ase']] == TRUE){
            model_subset = c(model_subset, name)
          }
        }
      }
      else{
        # Add all models
        for (name in names(private$get_models())){
          model_subset = c(model_subset, name)
        }
      }
      
      return(model_subset)
      
      },
    
    #### General Public Methods ----
    
    #' @description Add models to the existing object
    #' @param mdl_list The list of new models to add
    #' @param batch_size The batch size to use if model is using sliding ASE calculations
    add_models = function(mdl_list, batch_size = -1){
      if (length(unique(names(mdl_list))) != length(names(mdl_list))){
        stop("The model names in the provided list contain duplicates. Please fix and rerun.")
      }
      
      existing = names(private$models)
      if (any(names(mdl_list) %in% existing)){
        print(names(mdl_list)[names(mdl_list) %in% existing])
        stop("The model names above already exist in this comparison object. Please provide unique names.")
      }
      
      mdl_list = private$clean_model_input(mdl_list, batch_size)
      
      if (all(is.na(private$models))){
        private$models = mdl_list
      }
      else{
        private$models = c(private$models, mdl_list)
      }
      
      ## We are setting the batch size here since we want to allow the users
      ## to add models even after the object has been created.
      
      ## If we add models during initialization, we will always pass value (or NA) for batch_size, so
      ## it will never be = -1. It will be equal to -1 only when user adds model after initializing the 
      ## object, in which case, we pass the internal batch size to the setter (which warns user if needed)
      if(!is.na(batch_size) & batch_size == -1){
        self$set_batch_size(self$get_batch_size())
      }
      else{
        self$set_batch_size(batch_size)
      }
      
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
    },
    
    
    #' @description Computes the Metrics for the models
    #' @param step_n.ahead If TRUE, for rolling window calculations, the step size 
    #'                     will be equal to n.ahead, else it will be equal to 1
    compute_metrics = function(step_n.ahead = TRUE){
      for (name in names(private$get_models())){

        if (private$models[[name]][['metric_has_been_computed']] == FALSE){
          
          if (private$get_verbose() >= 1){
            cat(paste("\nComputing metrics for: ", name, "\n"))
          }

          res = private$get_sliding_ase_results(name = name, step_n.ahead = step_n.ahead)
          
          ## Inplace
          # private$models[[name]][['ASEs']] = res$ASEs
          # private$models[[name]][['time_test_start']] = res$time_test_start
          # private$models[[name]][['time_test_end']] = res$time_test_end
          # private$models[[name]][['batch_num']] = res$batch_num
          # 
          # private$models[[name]][['f']] = res$f
          # private$models[[name]][['ll']] = res$ll
          # private$models[[name]][['ul']] = res$ul
          # private$models[[name]][['time.forecasts']] = res$time.forecasts
          
          private$models[[name]]$ASEs = res$ASEs
          private$models[[name]]$time_test_start = res$time_test_start
          private$models[[name]]$time_test_end = res$time_test_end
          private$models[[name]]$batch_num = res$batch_num
          
          private$models[[name]]$f = res$f
          private$models[[name]]$ll = res$ll
          private$models[[name]]$ul = res$ul
          private$models[[name]]$time.forecasts = res$time.forecasts


          # private$models[[name]][['metric_has_been_computed']] = TRUE
          private$models[[name]]$metric_has_been_computed = TRUE

        }
        else{
          warning(paste("Metrics have already been computed for Model: '", name, "'. These will not be computed again."))
        }
      }
    },
    
    #' @description Plots the simple forecast for each model
    #' @param lastn If TRUE, this will plot the forecasts forthe last n.ahead values of the realization (Default: FALSE)
    #' @param newxreg The future exogenous variable values. Applicable to models that require the values of the new exogenous variables to be provided for future forecasts, e.g. nnfor::mlp()
    #' @param limits If TRUE, this will also plot the lower and upper limits of the forecasts (Default: FALSE)
    #' @param zoom A number indicating how much to zoom into the plot. 
    #'             For example zoom = 50 will only plot the last 50 points of the realization
    #'             Useful for cases where realizations that are long and n.ahead is small.
    #' @param plot If FALSE the plots are not plotted; useful when you want to just return the data (Default = TRUE)
    plot_simple_forecasts = function(lastn = FALSE, newxreg = NA, limits = FALSE, zoom = NA, plot = TRUE){
      
      forecasts = private$compute_simple_forecasts_with_validation(lastn = lastn, newxreg = newxreg)
        
      results = forecasts %>%   
        dplyr::add_row(Model = "Actual",
                       Time = seq_along(self$get_data_var_interest()),
                       f = self$get_data_var_interest(),
                       ll = self$get_data_var_interest(),
                       ul = self$get_data_var_interest())
      
      
      if (!is.na(zoom)){
        zoom = private$validate_zoom(zoom)
        
        n = private$get_len_x()
        start = n - zoom + 1
        
        results = results %>% 
          dplyr::filter(Time >= start)
      }
      
      if (plot == TRUE){  
        p = ggplot2::ggplot() +
          ggplot2::geom_line(results %>% dplyr::filter(Model == "Actual"), mapping = ggplot2::aes(x=Time, y=f, color = Model), size = 1) +
          ggplot2::geom_line(results %>% dplyr::filter(Model != "Actual"), mapping = ggplot2::aes(x=Time, y=f, color = Model), size = 0.75) +
          ggplot2::ylab("Simple Forecasts")
        
        if (limits == TRUE){
          p = p + 
            ggplot2::geom_line(results, mapping = ggplot2::aes(x=Time, y=ll, color = Model), linetype = "dashed", size = 0.5) +
            ggplot2::geom_line(results, mapping = ggplot2::aes(x=Time, y=ul, color = Model), linetype = "dashed", size = 0.5)
        }
        
        print(p)
      }
      
      return(list(forecasts = forecasts, plot_data = results))
      
    },
    
    #' @description Plots the forecasts per batch for all models
    #' @param only_sliding If TRUE, this will only plot the batch forecasts 
    #'                     for the models that used window ASE calculations
    #' @param plot If FALSE the plots are not plotted; useful when you want to just return the data (Default = TRUE)
    #' @param silent If FALSE, any warnings are suppressed
    plot_batch_forecasts = function(only_sliding = TRUE, plot = TRUE, silent = FALSE){
      if (only_sliding == TRUE & private$any_sliding_ase() == FALSE){
        if (silent == FALSE){
          message("None of your models are using a sliding ASE calculation, hence nothing will be plotted")
        }
      }
      else{
        results.forecasts = self$get_tabular_metrics(ases = FALSE)
        
        model_subset = c("Realization")
        if (only_sliding){
          for (name in names(private$get_models())){
            if (private$models[[name]][['sliding_ase']] == TRUE){
              model_subset = c(model_subset, name)
            }
          }
        }
        else{
          # Add all models
          for (name in names(private$get_models())){
            model_subset = c(model_subset, name)
          }
        }
        
        results.forecasts = results.forecasts %>% 
          dplyr::filter(Model %in% model_subset)
        
        # https://stackoverflow.com/questions/9968975/make-the-background-of-a-graph-different-colours-in-different-regions
        
        # Get Batch Boundaries
        results.ases = self$get_tabular_metrics(ases = TRUE)
        if (private$any_sliding_ase()){
          for (name in names(private$get_models())){
            if (private$models[[name]][['sliding_ase']] == TRUE){
              results.batches = results.ases %>% 
                  dplyr::filter(Model == name)
              break()
            }
          }
        }
        else{
          # No model has sliding ASE, so just pick the 1st one
          for (name in names(private$get_models())){
            results.batches = results.ases %>% 
              dplyr::filter(Model == name)
            break()
          }
        }
        
        rects = data.frame(xstart = results.batches[['Time_Test_Start']],
                           xend = results.batches[['Time_Test_End']],
                           Batch = rep(1, length(results.batches[['Batch']])))
        
        if (plot == TRUE){
          p = ggplot2::ggplot() + 
            ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
            ggplot2::geom_line(results.forecasts %>% dplyr::filter(Model == 'Realization'), mapping = ggplot2::aes(x = Time, y = f, color = Model), size = 1) +
            ggplot2::geom_line(results.forecasts %>% dplyr::filter(Model != 'Realization'), mapping = ggplot2::aes(x = Time, y = f, color = Model), size = 0.75) +
            ggplot2::ylab("Forecasts")
          
          print(p) 
        
          p = ggplot2::ggplot() +
            ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
            ggplot2::geom_line(results.forecasts %>% dplyr::filter(Model == 'Realization'), mapping = ggplot2::aes(x=Time, y=ll, color = Model), size = 1) + 
            ggplot2::geom_line(results.forecasts %>% dplyr::filter(Model == 'Realization'), mapping = ggplot2::aes(x=Time, y=ll, color = Model), size = 1) + 
            ggplot2::geom_line(results.forecasts %>% dplyr::filter(Model != 'Realization'), mapping = ggplot2::aes(x=Time, y=ll, color = Model), size = 0.75) + 
            ggplot2::geom_line(results.forecasts %>% dplyr::filter(Model != 'Realization'), mapping = ggplot2::aes(x=Time, y=ul, color = Model), size = 0.75) +
            ggplot2::ylab("Upper and Lower Forecast Limits (95%)")
          
          print(p)
        }
        
        return(list(forecasts = results.forecasts, batch_rects = rects))
      }
      
    },
    
    #' @description Plots the ASEs per batch for all models
    #' @param only_sliding If TRUE, this will only plot the ASEs for
    #'                     the models that used window ASE calculations
    #' @param plot If FALSE the plots are not plotted; useful when you want to just return the data (Default = TRUE)                      
    #' @param silent If FALSE, any warnings are suppressed
    plot_batch_ases = function(only_sliding = TRUE, plot = TRUE, silent = FALSE){
      if (only_sliding == TRUE & private$any_sliding_ase() == FALSE){
        if (silent == FALSE){
          message("None of your models are using a sliding ASE calculation, hence nothing will be plotted")
        }
      }
      else{
        requireNamespace("patchwork")
        
        model_subset = c()
        
        if (only_sliding == TRUE){
          for (name in names(private$get_models())){
            if (private$models[[name]][['sliding_ase']] == TRUE){
              model_subset = c(model_subset, name)
            }
          }
        }
        else{
          # Add all models
          for (name in names(private$get_models())){
            model_subset = c(model_subset, name)
          }
        }
        
        ASEs = self$get_tabular_metrics(ases = TRUE) %>% 
          dplyr::filter(Model %in% model_subset) %>%  
          tidyr::gather("Index", "Time", -Model, -ASE, -Batch) 
        
        all_time = NA
        
        for (name in model_subset){
          if (all(is.na(all_time))){
            all_time = data.frame(Time = seq(1, private$get_len_x()),
                                  Model = rep(name, private$get_len_x()),
                                  ASE = 0)
          }
          else{
            all_time = rbind(all_time, data.frame(Time = seq(1, private$get_len_x()),
                                                  Model = rep(name, private$get_len_x()),
                                                  ASE = 0))
          }
          
          all_time = all_time %>%
            dplyr::mutate_if(is.factor, as.character)
        }
        
        results = dplyr::left_join(all_time, ASEs, by = c("Time", "Model")) %>%
          dplyr::mutate(ASE = ASE.x + ASE.y) %>% 
          dplyr::group_by(Model) %>% 
          tidyr::fill(.data$ASE, .direction = "down")
        
        data = data.frame(Time = seq(1, private$get_len_x()), Data = self$get_data_var_interest())
        
        if (plot == TRUE){
          g1 = ggplot2::ggplot() + 
            ggplot2::geom_line(data, mapping = ggplot2::aes(x = Time, y = Data), size = 1)
        
          g2 = ggplot2::ggplot() +
            ggplot2::geom_line(results, mapping = ggplot2::aes(x = Time, y = ASE, color = Model), size = 1)
        
          print(g1/g2)
        }
        
        return(list(data = data, ASEs = results))
      }
    },
    
    #' @description Plots the boxplot of the ASE values for the models
    #' @param plot If FALSE the plots are not plotted; useful when you want to just return the data (Default = TRUE)
    plot_boxplot_ases = function(plot = TRUE){
      results = self$get_tabular_metrics()
      
      if (plot == TRUE){
        p = ggplot2::ggplot(results, ggplot2::aes(x = Model, y = ASE, color = Model)) + 
          ggplot2::geom_boxplot() + 
          ggplot2::stat_summary(fun = mean, colour="darkred", geom="point", 
                                shape=18, size=3, show.legend = FALSE) +
          ggplot2::coord_flip()
        print(p)
      }
      
      return(results)
    },
    
    #' @description Statistically compares the ASE values of the models using 
    #' ANOVA and Tukey Adjustment for multiple comparison
    #' @return The results of the ANOVA test
    statistical_compare = function(){
      if (private$any_sliding_ase()){
        results = self$get_tabular_metrics(only_sliding = TRUE)
      
        # Analysis of Variance
        res.aov = aov(ASE ~ Model, data = results)
        print(summary(res.aov))
        cat("\n\n")
        
        # Tukey Adjustment
        print(TukeyHSD(res.aov))
        
        return(res.aov)
      }
      else{
        warning("None of the models are using sliding ASE, hence statistical comparison is not possible")
      }
      
    },
    
    #' @description Gets the metrics and results in tabular format
    #' @param only_sliding If TRUE, this will only get results for models that use a 
    #'                     sliding ASE calculation method. (Default: FALSE)
    #' @param ases If TRUE returns the ASE values for each batch. If FALSE returns the 
    #'             forecasts, and the lower and upper limits asscoiated with the forecasts
    get_tabular_metrics = function(only_sliding = FALSE, ases = TRUE){
      if (ases == TRUE){
        results = dplyr::tribble(~Model, ~ASE, ~Time_Test_Start, ~Time_Test_End, ~Batch) 
      }
      else {
        results = dplyr::tribble(~Model, ~Time, ~f, ~ll, ~ul) 
      }
      
      model_names = c()
      
      for (name in names(private$get_models())){
        if (private$models[[name]][['metric_has_been_computed']] == TRUE){
          if(only_sliding == TRUE){
            if (private$models[[name]][['sliding_ase']] == TRUE){
              model_names = c(model_names, name)
            }
          }
          else{
            model_names = c(model_names, name)
          }
        }
        else{
          warning(paste("Metrics have not been computed for Model: '", name, "'. These will not be plotted."))
        }
      }
      
      for (name in model_names){    
        if (ases == TRUE){
          results = results %>% dplyr::add_row(Model = name,
                                               ASE = private$models[[name]][['ASEs']],
                                               Time_Test_Start = private$models[[name]][['time_test_start']],
                                               Time_Test_End = private$models[[name]][['time_test_end']],
                                               Batch = private$models[[name]][['batch_num']])
        }
        
        else{
          results = results %>% dplyr::add_row(Model = name,
                                               Time = private$models[[name]][['time.forecasts']],
                                               f = private$models[[name]][['f']],
                                               ll = private$models[[name]][['ll']],
                                               ul = private$models[[name]][['ul']])
          
        }
      }
      
      if (ases == FALSE){
        # Add the realization as well
        results = results %>% dplyr::add_row(Model = "Realization",
                                             Time = seq(1, private$get_len_x(), 1),
                                             f = self$get_data_var_interest(),
                                             ll = self$get_data_var_interest(),
                                             ul = self$get_data_var_interest())
      }
      
      return(results)
    },
    
    #' @description Returns the model Build Summary (if applicable)
    summarize_build = function(){
      stop("You are calling the 'summarize_build' method in the parent class. This should be implemented in the child class.")
    }
    
    
  ),
  
  
  #### Private Methods ----
  private = list(
    data = NA,
    models = NA,
    n.ahead = NA,
    batch_size = NA,
    step_n.ahead = NA,
    verbose = NA,
    
    set_data = function(data){
      if (all(is.na(data))){ stop("You have not provided the time series data. Please provide to continue.") }
      private$data = data
    },
    
    get_data_subset = function(col_names){
      stop("You are calling the 'get_data_subset' method in the parent class. This should be implemented in the child class.")
    },
    
    set_step_n.ahead = function(step_n.ahead){
      private$step_n.ahead = step_n.ahead
    },
    
    get_step_n.ahead = function(){ return(private$step_n.ahead)},
    
    get_len_x = function(){
      stop("You are calling the 'get_len_x' method in the parent class. This should be implemented in the child class.")
    },
    
    get_models = function(){
      return(private$models)
    },
    
    get_verbose = function(){
      return(private$verbose)
    },
    
    clean_model_input = function(mdl_list, batch_size){
      # If the inputs are missing sliding_ase, make it FALSE
      # Also add 'metric_has_been_computed' key
      for (name in names(mdl_list)){
        if (is.null(mdl_list[[name]][['sliding_ase']])){
          mdl_list[[name]][['sliding_ase']] = FALSE
        }
        
        mdl_list[[name]][['metric_has_been_computed']] = FALSE
      }
      
      return(mdl_list)
    },
    
    get_sliding_ase_results = function(name, step_n.ahead){
      stop("You are calling the 'get_sliding_ase_results' method in the parent class. This should be implemented in the child class.")
    },
    
    compute_simple_forecasts = function(lastn){
      stop("You are calling the 'compute_simple_forecasts' method in the parent class. This should be implemented in the child class.")  
    },
    
    compute_simple_forecasts_with_validation = function(lastn, newxreg){
      
      results = private$compute_simple_forecasts(lastn = lastn, newxreg = newxreg) %>% 
        assertr::verify(assertr::has_all_names("Model", "Time", "f", "ll", "ul"))
      
      return(results)
    },
    
    any_sliding_ase = function(){
      for (name in names(private$get_models())){
        if (private$get_models()[[name]][['sliding_ase']]){
          return(TRUE)
        }
      }
      return(FALSE)
    },
    
    set_n.ahead = function(n.ahead){
      private$n.ahead = n.ahead
    },
    
    set_batch_per_model = function(){
      for (name in names(private$get_models())){
        if (private$get_models()[[name]][['sliding_ase']]){
          private$models[[name]][['batch_size']] = self$get_batch_size()  ## Inplace, hence not using get_models
          private$models[[name]][['metric_has_been_computed']] = FALSE  ## Setting to FALSE since the batch size has changed 
        }
        else{
          private$models[[name]][['batch_size']] = NA  ## Inplace, hence not using get_models
          # Not setting 'metric_has_been_computed' to FALSE since resetting the batch size should not impact models that dont use sliding ASE
        }
      }
    },
    
    build_models  = function(verbose = 0){
      stop("You are calling the 'build_models' method in the parent class. This should be implemented in the child class.")
    },
    
    evaluate_xIC = function(){
      stop("You are calling the 'evaluate_xIC' method in the parent class. This should be implemented in the child class.")
    },
    
    evaluate_models = function(){
      # private$build_models(verbose = private$get_verbose())
      # private$evaluate_xIC()
      self$compute_metrics(step_n.ahead = private$get_step_n.ahead())
      print(self$summarize_build())  
    },
    
    validate_zoom = function(zoom){
      rvZoom = zoom
      if (!is.numeric(zoom) | zoom <= 0){
        warning(paste("The zoom value you have provided: ", zoom, " is not numeric or <= 0. No zoom will be used"))
        rvZoom = private$get_len_x()
      }
      if (zoom < self$get_n.ahead()){
        message(paste("The zoom value you have provided: ", zoom, " is less than the n.ahead value"))
      }
      return(rvZoom)
    }
    
  )
  
)

