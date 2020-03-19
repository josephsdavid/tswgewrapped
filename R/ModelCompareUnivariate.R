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
#' mdl_compare = ModelCompareUnivariate$new(x = airlog, mdl_list = models,
#'                                          n.ahead = 36, batch_size = 72)
#' # Plots the historgam of the ASE values for each model.
#' # This is especially useful when models using a sliding window for ASE calculations. 
#' mdl_compare$plot_histogram_ases()
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
  cloneable = TRUE,
  lock_objects=F,
  lock_class=F,
  
  #### Public Methods ----
  public=list(
    #### Constructor ----
    
    #' @description 
    #' Initialize an object to compare several Univatiate Time Series Models
    #' @param x Time Series Realization
    #' @param mdl_list A names list of all models (see format below)
    #' @param n.ahead The number of observations used to calculate ASE or forecast ahead
    #' @param batch_size If any of the models used sliding ase method,
    #'                   then this number indicates the batch size to use
    #' @param step_n.ahead If using sliding window, should batches be incremented by n.ahead
    #'                     (Default = TRUE)
    #' @return A new `ModelCompareUnivariate` object.
    initialize = function(x = NA, mdl_list, n.ahead = NA, batch_size = NA, step_n.ahead = TRUE)
    {
      # Add checks here
      if (all(is.na(x))){ stop("You have not provided the time series data. Please provide to continue.") }
      
      private$set_x(x = x)
      self$add_models(mdl_list)
      private$set_n.ahead(n.ahead)
      private$set_batch_size(batch_size)
      self$compute_metrics(step_n.ahead = step_n.ahead)
      
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the time series realization
    #' @return The Time Series Realization
    get_x = function(){return(private$x)},
    
    #' @description Returns the batch size value
    #' @return The Batch Size Value
    get_batch_size = function(){return(private$batch_size)},
    
    #' @description Returns the n.ahead value
    #' @return The n.ahead value
    get_n.ahead = function(){return(private$n.ahead)},
    
    #' @description Add models to the existing object
    #' @param mdl_list The list of new models to add
    add_models = function(mdl_list){
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
    },
    
    #### General Public Methods ----
    
    #' @description Computes the Metrics for the models
    #' @param step_n.ahead If TRUE, for rolling window calculations, the step size 
    #'                     will be equal to n.ahead, else it will be equal to 1
    compute_metrics = function(step_n.ahead = TRUE){
      for (name in names(private$get_models())){
        
        if (private$models[[name]][['metric_has_been_computed']] == FALSE){
          cat(paste("\n\n\nComputing metrics for: ", name, "\n"))
          
          res = sliding_ase(x = self$get_x(),
                            phi = private$get_models()[[name]][['phi']],
                            theta = private$get_models()[[name]][['theta']],
                            d = private$get_models()[[name]][['d']],
                            s = private$get_models()[[name]][['s']],
                            n.ahead = self$get_n.ahead(),
                            batch_size = private$get_models()[[name]][['batch_size']],
                            step_n.ahead = step_n.ahead)
          
          ## Inplace
          private$models[[name]][['ASEs']] = res$ASEs  
          private$models[[name]][['time_test_start']] = res$time_test_start
          private$models[[name]][['time_test_end']] = res$time_test_end
          private$models[[name]][['batch_num']] = res$batch_num
          private$models[[name]][['f']] = res$f
          private$models[[name]][['ll']] = res$ll
          private$models[[name]][['ul']] = res$ul
          private$models[[name]][['time.forecasts']] = res$time.forecasts
          
          
          private$models[[name]][['metric_has_been_computed']] = TRUE
          
        }
        else{
          warning(paste("Metrics have already been computed for Model: '", name, "'. These will not be computed again."))
        }
      }
    },
    
    #' @description Plots the simple forecast for each model
    #' @param lastn If TRUE, this will plot the forecasts forthe last n.ahead values of the realization (Default: FALSE)
    #' @param limits If TRUE, this will also plot the lower and upper limits of the forecasts (Default: FALSE)
    plot_simple_forecasts = function(lastn = FALSE, limits = FALSE){
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
        forecast = tswge::fore.aruma.wge(x = self$get_x(),
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
      
      results = results %>%  dplyr::add_row(Model = "Actual",
                                            Time = seq_along(self$get_x()),
                                            f = self$get_x(),
                                            ll = self$get_x(),
                                            ul = self$get_x()
      )
      
      p = ggplot2::ggplot() +
        ggplot2::geom_line(results, mapping = ggplot2::aes(x=Time, y=f, color = Model), size = 0.4) +
        ggplot2::ylab("Simple Forecasts")
      
      if (limits == TRUE){
        p = p + 
          ggplot2::geom_line(results, mapping = ggplot2::aes(x=Time, y=ll, color = Model), linetype = "dashed", size = 0.2) +
          ggplot2::geom_line(results, mapping = ggplot2::aes(x=Time, y=ul, color = Model), linetype = "dashed", size = 0.2)
      }
      
      print(p)
      
    },
    
    #' @description Plots the forecasts per batch for all models
    #' @param only_sliding If TRUE, this will only plot the batch forecasts 
    #'                     for the models that used window ASE calculations
    plot_batch_forecasts = function(only_sliding = TRUE){
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
      for (name in names(private$get_models())){
        if (private$models[[name]][['sliding_ase']] == TRUE){
          results.batches = results.ases %>% 
            dplyr::filter(Model == name)
          break()
        }
      }
      
      rects = data.frame(xstart = results.batches[['Time_Test_Start']],
                         xend = results.batches[['Time_Test_End']],
                         Batch = rep(1, length(results.batches[['Batch']])))
      
      
      p = ggplot2::ggplot() + 
        ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
        ggplot2::geom_line(results.forecasts, mapping = ggplot2::aes(x = Time, y = f, color = Model)) +
        ggplot2::ylab("Forecasts")
      
      print(p) 
      
      
      p = ggplot2::ggplot() +
        ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
        ggplot2::geom_line(results.forecasts, mapping = ggplot2::aes(x=Time, y=ll, color = Model)) + 
        ggplot2::geom_line(results.forecasts, mapping = ggplot2::aes(x=Time, y=ul, color = Model)) +
        ggplot2::ylab("Upper and Lower Forecast Limits (95%)")
      
      print(p)
      
    },
    
    #' @description Plots the ASEs per batch for all models
    #' @param only_sliding If TRUE, this will only plot the ASEs for
    #'                     the models that used window ASE calculations
    plot_batch_ases = function(only_sliding = TRUE){
      
      requireNamespace("patchwork")
      
      model_subset = c()
      
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
        tidyr::fill(.data$ASE, .direction = "down")
      
      data = data.frame(Time = seq(1, private$get_len_x()), Data = self$get_x())
      
      g1 = ggplot2::ggplot(data, ggplot2::aes(x = Time, y = Data)) + 
        ggplot2::geom_line()
      
      g2 = ggplot2::ggplot(results, ggplot2::aes(x = Time, y = ASE, color = Model)) +
        ggplot2::geom_line()
      
      print(g1/g2)
    },
    
    #' @description Plots the histogram of the ASE values for the models
    plot_histogram_ases = function(){
      results = self$get_tabular_metrics()
      p = ggplot2::ggplot(results, ggplot2::aes(x = Model, y = ASE, color = Model)) + 
        ggplot2::geom_boxplot() + 
        ggplot2::coord_flip()
      print(p)
    },
    
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
        r = generate_multiple_realization(x = self$get_x(),
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
          
          table = white_noise_eval(private$models[[name]][['res']], p = p, q = q, model_name = name, lag.max = lag.max)
        
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
    
    #' @description Statistically compares the ASE values of the models using 
    #' ANOVA and Tukey Adjustment for multiple comparison
    #' @param only_sliding If TRUE, this will only include models that use a sliding ASE calculation method,
    #'                     i.e. those models that have multiple ASE values (Default: TRUE)
    #' @return The results of the ANOVA test
    statistical_compare = function(){
      results = self$get_tabular_metrics(only_sliding = TRUE)
      
      # Analysis of Variance
      res.aov = aov(ASE ~ Model, data = results)
      print(summary(res.aov))
      cat("\n\n")
      
      # Tukey Adjustment
      print(TukeyHSD(res.aov))
      
      return(res.aov)
      
    },
    
    #' @description Gets the metrics and results in tabular format
    #' @param only_sliding If TRUE, this will only get results for models that use a 
    #'                     sliding ASE calculation method. (Default: FALSE)
    #' @param ases If TRUE returns the ASE values for each batch. If FALSE returns the 
    #'             forecasts, and the lower and upper limits asscoiated with the forecasts
    get_tabular_metrics = function(only_sliding = FALSE, ases = TRUE){
      # 
      
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
                                             f = self$get_x(),
                                             ll = self$get_x(),
                                             ul = self$get_x())
      }
      
      return(results)
    }
    
    
  ),
  
  
  #### Private Methods ----
  private = list(
    x = NA,
    models = NA,
    n.ahead = NA,
    batch_size = NA,
    
    set_x = function(x){private$x = x},
    
    get_len_x = function(){
      return(length(self$get_x()))
    },
    
    get_models = function(){
      return(private$models)
    },
    
    clean_model_input = function(mdl_list){
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
        if (is.null(mdl_list[[name]][['sliding_ase']])){
          mdl_list[[name]][['sliding_ase']] = FALSE
        }
        
        mdl_list[[name]][['metric_has_been_computed']] = FALSE
        
      }
      
      return(mdl_list)
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
        }
        else{
          private$models[[name]][['batch_size']] = NA  ## Inplace, hence not using get_models
        }
      }
    },
    
    set_batch_size = function(batch_size){
      if (private$any_sliding_ase() & is.na(batch_size)){
        stop("You have provided models that require sliding ASE calculations, but the batch size has been set to NA. Please provide an appropriate value to proceed.")
      }
      private$batch_size = batch_size
      private$set_batch_per_model()
    }
    
  )
  
)

