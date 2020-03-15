#' R6 class ModelCompareUnivariate
#' @description 
#' Initialize an object to compare several Univatiate Time Series Models
#' @param x Time Series Realization
#' @param mdl_list A names list of all models (see format below)
#' @param n.ahead The number of observations used to calculate ASE or forecast ahead
#' @param batch_size If any of the models used sliding ase method,
#'                   then this number indicates the batch size to use
#' @param step_n.ahead If using sliding window, should batches be incremented by n.ahead
#'                     (Default = TRUE)
#' @usage ModelCompareUnivariate$new(x = airlog, mdl_list = models,
#'                            n.ahead = 36, batch_size = 72)
#' @return A new `ModelCompareUnivariate` object.
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
#' mdl_compare$plot_forecasts(only_sliding = TRUE)
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
    x = NA,
    #num_models = NA,
    models = NA,
    n.ahead = NA,
    batch_size = NA,

    #### Constructor ----
    
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
    
    get_x = function(){return(self$x)},
    
    get_batch_size = function(){return(self$batch_size)},
    get_n.ahead = function(){return(self$n.ahead)},
    
    
    add_models = function(mdl_list){
      if (length(unique(names(mdl_list))) != length(names(mdl_list))){
        stop("The model names in the provided list contain duplicates. Please fix and rerun.")
      }
      
      existing = names(self$models)
      if (any(names(mdl_list) %in% existing)){
        print(names(mdl_list)[names(mdl_list) %in% existing])
        stop("The model names above already exist in this comparison object. Please provide unique names.")
      }
      
      mdl_list = private$clean_model_input(mdl_list)
      
      if (all(is.na(self$models))){
        self$models = mdl_list
      }
      else{
        self$models = c(self$models, mdl_list)
      }
    },
    
    remove_models = function(x){
      stop("This has not been implemented yet.")  
    },
    
    #### General Public Methods ----
    
    compute_metrics = function(step_n.ahead = TRUE){
      for (name in names(private$get_models())){
        
        if (self$models[[name]][['metric_has_been_computed']] == FALSE){
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
          self$models[[name]][['ASEs']] = res$ASEs  
          self$models[[name]][['time_test_start']] = res$time_test_start
          self$models[[name]][['time_test_end']] = res$time_test_end
          self$models[[name]][['batch_num']] = res$batch_num
          self$models[[name]][['f']] = res$f
          self$models[[name]][['ll']] = res$ll
          self$models[[name]][['ul']] = res$ul
          self$models[[name]][['time.forecasts']] = res$time.forecasts
          
          
          self$models[[name]][['metric_has_been_computed']] = TRUE
          
        }
        else{
          warning(paste("Metrics have already been computed for Model: '", name, "'. These will not be computed again."))
        }
      }
    },
    
    
    plot_histogram_ases = function(){
      results = self$get_tabular_metrics()
      p = ggplot2::ggplot(results, ggplot2::aes(x = Model, y = ASE, color = Model)) + 
        ggplot2::geom_boxplot() + 
        ggplot2::coord_flip()
      print(p)
    },
    
    plot_forecasts = function(only_sliding = TRUE){
      results.forecasts = self$get_tabular_metrics(ases = FALSE)
      
      model_subset = c("Realization")
      if (only_sliding){
        for (name in names(private$get_models())){
          if (self$models[[name]][['sliding_ase']] == TRUE){
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
      
      # library(magrittr)
      # library(dplyr)
      
      results.forecasts = results.forecasts %>% 
        dplyr::filter(Model %in% model_subset)
      
      # https://stackoverflow.com/questions/9968975/make-the-background-of-a-graph-different-colours-in-different-regions
      
      # Get Batch Boundaries
      results.ases = self$get_tabular_metrics(ases = TRUE)
      for (name in names(private$get_models())){
        if (self$models[[name]][['sliding_ase']] == TRUE){
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
    
    plot_simple_forecasts = function(){
      results = dplyr::tribble(~Model, ~Time, ~f, ~ll, ~ul)
      from = private$get_len_x() + 1
      to = private$get_len_x() + self$get_n.ahead()

      for (name in names(private$get_models())){
        forecast = tswge::fore.aruma.wge(x = self$get_x(),
                                  phi = private$get_models()[[name]][['phi']],
                                  theta = private$get_models()[[name]][['theta']],
                                  d = private$get_models()[[name]][['d']],
                                  s = private$get_models()[[name]][['s']], 
                                  n.ahead = self$get_n.ahead(),
                                  lastn = FALSE, plot = FALSE)
        
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
        ggplot2::geom_line(results, mapping = ggplot2::aes(x=Time, y=f, color = Model)) +
        ggplot2::ylab("Simple Forecasts")

      print(p)
      
    },
    
    plot_multiple_realizations = function(n.realizations = 4, lag.max = 25, seed = NA, plot = c("all"), scales = 'free_y'){
      final.data = NA
      final.results = NA
      
      for (name in names(private$get_models())){
        r = generate_multiple_realization(x = self$get_x(),
                                          phi = self$models[[name]][['phi']],
                                          theta = self$models[[name]][['theta']],
                                          d = self$models[[name]][['d']],
                                          s = self$models[[name]][['s']],
                                          vara = self$models[[name]][['vara']],
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
    
    get_tabular_metrics = function(only_sliding = FALSE, ases = TRUE){
      # ases = TRUE returns the ASE values for each batch
      # ases = FALSE returns the forecasts, and the lower limits and upper limits asscoiated with the forecasts
      
      if (ases == TRUE){
        results = dplyr::tribble(~Model, ~ASE, ~Time_Test_Start, ~Time_Test_End, ~Batch) 
      }
      else {
        results = dplyr::tribble(~Model, ~Time, ~f, ~ll, ~ul) 
      }
      
      model_names = c()
      
      for (name in names(private$get_models())){
        if (self$models[[name]][['metric_has_been_computed']] == TRUE){
          if(only_sliding == TRUE){
            if (self$models[[name]][['sliding_ase']] == TRUE){
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
      
      # library(magrittr)
      
      for (name in model_names){    
        if (ases == TRUE){
          results = results %>% dplyr::add_row(Model = name,
                                               ASE = self$models[[name]][['ASEs']],
                                               Time_Test_Start = self$models[[name]][['time_test_start']],
                                               Time_Test_End = self$models[[name]][['time_test_end']],
                                               Batch = self$models[[name]][['batch_num']])
        }
        
        else{
          results = results %>% dplyr::add_row(Model = name,
                                               Time = self$models[[name]][['time.forecasts']],
                                               f = self$models[[name]][['f']],
                                               ll = self$models[[name]][['ll']],
                                               ul = self$models[[name]][['ul']])
          
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
    set_x = function(x){self$x = x},
    
    get_len_x = function(){
      return(length(self$get_x()))
    },
    
    get_models = function(){
      return(self$models)
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
      self$n.ahead = n.ahead
    },
    
    set_batch_per_model = function(){
      for (name in names(private$get_models())){
        if (private$get_models()[[name]][['sliding_ase']]){
          self$models[[name]][['batch_size']] = self$get_batch_size()  ## Inplace, hence not using get_models
        }
        else{
          self$models[[name]][['batch_size']] = NA  ## Inplace, hence not using get_models
        }
      }
    },
    
    set_batch_size = function(batch_size){
      if (private$any_sliding_ase() & is.na(batch_size)){
        stop("You have provided models that require sliding ASE calculations, but the batch size has been set to NA. Please provide an appropriate value to proceed.")
      }
      self$batch_size = batch_size
      private$set_batch_per_model()
    }
    
  )
  
)

