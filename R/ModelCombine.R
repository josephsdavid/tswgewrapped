#' @title R6 class ModelCompareMultivariate
#' 
#' @export
ModelCombine = R6::R6Class(
  classname = "ModelCombine",
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
    #' @param uni_models A vector of ModelCompareUnivariate objects
    #' @param var_models A vector of ModelCompareMultivariateVAR objects
    #' @param mlp_models A vector of ModelCompareNNforCaret objects (only picks the best caret model)
    #' @param verbose How much to print during the process (Default = 0)
    #' @return A new `ModelCombine` object.
    initialize = function(data = NA, var_interest = NA, uni_models = NA, var_models = NA, mlp_models = NA, verbose = 0)
    {
      self$set_verbose(verbose = verbose)
      private$set_ensemble_built(FALSE)
      private$set_data(data = data)
      private$set_var_interest(var_interest = var_interest)
      private$set_uni_compare_objects(models = uni_models)
      private$set_var_compare_objects(models = var_models)
      private$set_mlp_compare_objects(models = mlp_models)
      private$validate_models()
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
    set_verbose = function(verbose = 0){ private$verbose = verbose },
    
    #### General Public Methods ----
    
    #' @description Plots the simple forecast for each model
    #' @param lastn If TRUE, this will plot the forecasts forthe last n.ahead values of the realization (Default: FALSE)
    #' @param newxreg The future exogenous variable values to be used for prediction.
    #'                Applicable to models that require the values of the new exogenous variables to be provided for future forecasts, e.g. nnfor::mlp()
    #' @param limits If TRUE, this will also plot the lower and upper limits of the forecasts (Default: FALSE)
    #' @param zoom A number indicating how much to zoom into the plot. 
    #'             For example zoom = 50 will only plot the last 50 points of the realization
    #'             Useful for cases where realizations that are long and n.ahead is small.
    plot_simple_forecasts = function(lastn = FALSE, newxreg = NA, limits = FALSE, zoom = NA){
      
      forecasts = data.frame()
      
      mlp_compare_objects = private$get_mlp_compare_objects()
      if (length(mlp_compare_objects) >= 1 & lastn == TRUE){
        stop(paste0("Your '", self$classname, "' object has a ModelCompareNNforCaret object which does not support plotting simple forecasts with lastn = TRUE. Please make lastn = FALSE and rerun with xreg passed."))
      }
      for (i in seq_along(mlp_compare_objects)){
        subset_results = mlp_compare_objects[[i]]$plot_simple_forecasts(lastn = lastn, newxreg = newxreg, limits = limits, zoom = zoom, plot = FALSE)
        
        filtered = subset_results$plot_data %>%  
          private$filter_best_caret_model(caret_compare_object = mlp_compare_objects[[i]])
        
        forecasts = dplyr::bind_rows(forecasts, filtered) 
      }
      
      uni_compare_objects = private$get_uni_compare_objects()
      for (i in seq_along(uni_compare_objects)){
        subset_results = uni_compare_objects[[i]]$plot_simple_forecasts(lastn = lastn, newxreg = newxreg, limits = limits, zoom = zoom, plot = FALSE)
        forecasts = dplyr::bind_rows(forecasts, subset_results$plot_data)
      }
      
      var_compare_objects = private$get_var_compare_objects()
      for (i in seq_along(var_compare_objects)){
        subset_results = var_compare_objects[[i]]$plot_simple_forecasts(lastn = lastn, newxreg = newxreg, limits = limits, zoom = zoom, plot = FALSE)
        forecasts = dplyr::bind_rows(forecasts, subset_results$plot_data)
      }
      
      p = ggplot2::ggplot() +
        ggplot2::geom_line(forecasts %>% dplyr::filter(Model == "Actual"), mapping = ggplot2::aes(x=Time, y=f, color = Model), size = 1) +
        ggplot2::geom_line(forecasts %>% dplyr::filter(Model != "Actual"), mapping = ggplot2::aes(x=Time, y=f, color = Model), size = 0.75) +
        ggplot2::ylab("Simple Forecasts")
      
      if (limits == TRUE){
        p = p + 
          ggplot2::geom_line(forecasts, mapping = ggplot2::aes(x=Time, y=ll, color = Model), linetype = "dashed", size = 0.5) +
          ggplot2::geom_line(forecasts, mapping = ggplot2::aes(x=Time, y=ul, color = Model), linetype = "dashed", size = 0.5)
      }
      
      print(p)
      
      return(forecasts)

    },
    
    #' @description Plots the forecasts per batch for all models
    #' @param only_sliding If TRUE, this will only plot the batch forecasts 
    #'                     for the models that used window ASE calculations
    plot_batch_forecasts = function(only_sliding = TRUE){
      
      any_sliding = FALSE
      
      forecasts = data.frame()
      
      uni_compare_objects = private$get_uni_compare_objects()
      for (i in seq_along(uni_compare_objects)){
        subset_results = uni_compare_objects[[i]]$plot_batch_forecasts(only_sliding = only_sliding, plot = FALSE, silent = TRUE)
        forecasts = dplyr::bind_rows(forecasts, subset_results$forecasts)
      }
      
      var_compare_objects = private$get_var_compare_objects()
      for (i in seq_along(var_compare_objects)){
        subset_results = var_compare_objects[[i]]$plot_batch_forecasts(only_sliding = only_sliding, plot = FALSE, silent = TRUE)
        forecasts = dplyr::bind_rows(forecasts, subset_results$forecasts)
      }
      
      mlp_compare_objects = private$get_mlp_compare_objects()
      for (i in seq_along(mlp_compare_objects)){
        subset_results = mlp_compare_objects[[i]]$plot_batch_forecasts(only_sliding = only_sliding, plot = FALSE, silent = TRUE)
        
        filtered = subset_results$forecasts %>%  
          private$filter_best_caret_model(caret_compare_object = mlp_compare_objects[[i]])
        
        forecasts = dplyr::bind_rows(forecasts, filtered) 
        
        ## MLP Caret is guaranteed to have batches
        rects = subset_results$batch_rects
      }
      
      if (nrow(forecasts %>% dplyr::filter(Model != 'Realization')) >= 1){
        any_sliding = TRUE
      }
      
      if (only_sliding == TRUE & any_sliding == FALSE){
        message("None of your models are using a sliding ASE calculation, hence nothing will be plotted")
      }
      else{
        p = ggplot2::ggplot() + 
          ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
          ggplot2::geom_line(forecasts %>% dplyr::filter(Model == 'Realization'), mapping = ggplot2::aes(x = Time, y = f, color = Model), size = 1) +
          ggplot2::geom_line(forecasts %>% dplyr::filter(Model != 'Realization'), mapping = ggplot2::aes(x = Time, y = f, color = Model), size = 0.75) +
          ggplot2::ylab("Forecasts")
        
        print(p) 
        
        p = ggplot2::ggplot() +
          ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Batch), alpha = 0.1, show.legend = FALSE) +  
          ggplot2::geom_line(forecasts %>% dplyr::filter(Model == 'Realization'), mapping = ggplot2::aes(x=Time, y=ll, color = Model), size = 1) + 
          ggplot2::geom_line(forecasts %>% dplyr::filter(Model == 'Realization'), mapping = ggplot2::aes(x=Time, y=ll, color = Model), size = 1) + 
          ggplot2::geom_line(forecasts %>% dplyr::filter(Model != 'Realization'), mapping = ggplot2::aes(x=Time, y=ll, color = Model), size = 0.75) + 
          ggplot2::geom_line(forecasts %>% dplyr::filter(Model != 'Realization'), mapping = ggplot2::aes(x=Time, y=ul, color = Model), size = 0.75) +
          ggplot2::ylab("Upper and Lower Forecast Limits (95%)")
        
        print(p)
      }
    },
    
    #' @description Plots the ASEs per batch for all models
    #' @param only_sliding If TRUE, this will only plot the ASEs for
    #'                     the models that used window ASE calculations
    plot_batch_ases = function(only_sliding = TRUE){
      
      any_sliding = FALSE
      
      ASEs = data.frame()
        
      uni_compare_objects = private$get_uni_compare_objects()
      for (i in seq_along(uni_compare_objects)){
        subset_results = uni_compare_objects[[i]]$plot_batch_ases(only_sliding = only_sliding, plot = FALSE, silent = TRUE)
        ASEs = dplyr::bind_rows(ASEs, subset_results$ASEs)
      }
      
      var_compare_objects = private$get_var_compare_objects()
      for (i in seq_along(var_compare_objects)){
        subset_results = var_compare_objects[[i]]$plot_batch_ases(only_sliding = only_sliding, plot = FALSE, silent = TRUE)
        ASEs = dplyr::bind_rows(ASEs, subset_results$ASEs)
      }
      
      mlp_compare_objects = private$get_mlp_compare_objects()
      for (i in seq_along(mlp_compare_objects)){
        subset_results = mlp_compare_objects[[i]]$plot_batch_ases(only_sliding = only_sliding, plot = FALSE, silent = TRUE) 
        
        filtered = subset_results$ASEs %>%  
          private$filter_best_caret_model(caret_compare_object = mlp_compare_objects[[i]])
        
        ASEs = dplyr::bind_rows(ASEs, filtered)
      }
      
      if (nrow(ASEs) >= 1){
        any_sliding = TRUE
      }
      
      if (only_sliding == TRUE & any_sliding == FALSE){
        message("None of your models are using a sliding ASE calculation, hence nothing will be plotted")
      }
      else{
        requireNamespace("patchwork")
        
        data = data.frame(Time = seq(1, private$get_len_x()), Data = self$get_data_var_interest())
        
        g1 = ggplot2::ggplot() + 
          ggplot2::geom_line(data, mapping = ggplot2::aes(x = Time, y = Data), size = 1)
        
        g2 = ggplot2::ggplot() +
          ggplot2::geom_line(ASEs, mapping = ggplot2::aes(x = Time, y = ASE, color = Model), size = 1)
        
        print(g1/g2)
      }
    },
    
    #' @description Plots the boxplot of the ASE values for the models
    plot_boxplot_ases = function(){
      results = self$get_tabular_metrics()
      p = ggplot2::ggplot(results, ggplot2::aes(x = Model, y = ASE, color = Model)) + 
        ggplot2::geom_boxplot() + 
        ggplot2::stat_summary(fun = mean, colour="darkred", geom="point", 
                               shape=18, size=3, show.legend = FALSE) +
        ggplot2::coord_flip()
      print(p)
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
      
      uni_compare_objects = private$get_uni_compare_objects()
      for (i in seq_along(uni_compare_objects)){
        subset_results = uni_compare_objects[[i]]$get_tabular_metrics(only_sliding = only_sliding, ases = ases)
        results = rbind(results, subset_results)
      }
      
      var_compare_objects = private$get_var_compare_objects()
      for (i in seq_along(var_compare_objects)){
        subset_results = var_compare_objects[[i]]$get_tabular_metrics(only_sliding = only_sliding, ases = ases)
        results = rbind(results, subset_results)
      }
      
      mlp_compare_objects = private$get_mlp_compare_objects()
      for (i in seq_along(mlp_compare_objects)){
        subset_results = mlp_compare_objects[[i]]$get_tabular_metrics(only_sliding = only_sliding, ases = ases) %>% 
          private$filter_best_caret_model(caret_compare_object = mlp_compare_objects[[i]])
        
        results = rbind(results, subset_results)
      }
        
      return(results)
    },
    
    #' @description Computes the simple forecasts using all the models
    #' @param lastn If TRUE, this will get the forecasts for the last n.ahead values of the realization (Default: FALSE). 
    #'              If there is a ModelCompareNNforCaret object passed to this object, then lastn must be TRUE.
    #' @param newxreg The future exogenous variable values to be used for prediction.
    #'                Applicable to models that require the values of the new exogenous variables to be provided for future forecasts, e.g. nnfor::mlp()
    #' @return The forecasted values
    compute_simple_forecasts = function(lastn = FALSE, newxreg = NA){
      
      forecasts = data.frame()
      
      mlp_compare_objects = private$get_mlp_compare_objects()
      if (length(mlp_compare_objects) >= 1 & lastn == TRUE){
        stop(paste0("Your '", self$classname, "' object has a ModelCompareNNforCaret object which does not support plotting simple forecasts with lastn = TRUE. Please make lastn = FALSE and rerun with xreg passed."))
      }
      for (i in seq_along(mlp_compare_objects)){
        subset_results = mlp_compare_objects[[i]]$plot_simple_forecasts(lastn = lastn, newxreg = newxreg, limits = FALSE, plot = FALSE)
        
        filtered = subset_results$forecasts %>%  
          private$filter_best_caret_model(caret_compare_object = mlp_compare_objects[[i]])
        
        forecasts = dplyr::bind_rows(forecasts, filtered) 
      }
      
      uni_compare_objects = private$get_uni_compare_objects()
      for (i in seq_along(uni_compare_objects)){
        subset_results = uni_compare_objects[[i]]$plot_simple_forecasts(lastn = lastn, newxreg = newxreg, limits = FALSE, plot = FALSE)
        forecasts = dplyr::bind_rows(forecasts, subset_results$forecasts)
      }
      
      var_compare_objects = private$get_var_compare_objects()
      for (i in seq_along(var_compare_objects)){
        subset_results = var_compare_objects[[i]]$plot_simple_forecasts(lastn = lastn, newxreg = newxreg, limits = FALSE, plot = FALSE)
        forecasts = dplyr::bind_rows(forecasts, subset_results$forecasts)
      }
      
      return(forecasts)
    },
    
    #' @description Creates an ensemble model based on all the models provided
    create_ensemble = function(cuts = NA){
      
      private$set_cuts(cuts)
      
      # print("Cuts")
      # print(private$get_cuts())
      
      data_for_model = self$get_tabular_metrics(only_sliding = TRUE, ases = FALSE) %>%
        dplyr::distinct() %>%  # Remove duplicate entries for Model = 'Realization'
        assertr::verify(assertr::has_all_names("Time", "Model", "f")) %>%
        tidyr::pivot_wider(id_cols = Time, names_from = Model, values_from = f) %>% 
        stats::na.omit() %>% 
        dplyr::select(-Time) %>% 
        private$add_cuts(cuts = private$get_cuts()) 
      
      if (!all(is.na(private$get_cuts()))){
        data_for_model = data_for_model %>% 
          dplyr::select(-Realization_cut)
      }
      
      print(str(data_for_model))
        
      glm_ensemble = glm(formula = Realization ~ ., data = data_for_model)
      
      if (private$get_verbose() >= 1){
        print(summary(glm_ensemble))
        par(ask = FALSE) 
        plot(glm_ensemble)  
      }
      
      private$set_ensemble_model(model = glm_ensemble)
      private$set_ensemble_built(TRUE)
      
    },
    
    #' @description Makes a prediction based on the ensemble model
    #' @param naive If TRUE, the ensemble will be a simple mean of the prediction of all the models
    #'              If FALSE, the ensemble will use a glm model created from the batch predictions of all the models
    #' @param comb If 'naive' = TRUE, how to combine the predictions. Allowed values are 'mean' or 'median'
    #' @param newxreg  The future exogenous variable values to be used for prediction.
    #'                 Applicable to models that require the values of the new exogenous variables to be provided for future forecasts, e.g. nnfor::mlp()             
    #' @return The predictions from each model along with the ensemble prediction                
    predict_ensemble = function(naive = FALSE, comb = 'median', newxreg = NA){
      
      if (naive == TRUE){
        if (comb != 'median' & comb != 'mean'){
          warning(paste0("You are using a naive model, but the value of comb is set to '", comb, "' . The allowed values are 'median' or 'mean'. This will be set to the default value of 'median'."))
          comb = 'median'
        }
      }
      
      forecasts = self$compute_simple_forecasts(lastn = FALSE, newxreg = newxreg) %>% 
        assertr::verify(assertr::has_all_names("Time", "Model", "f")) %>% 
        tidyr::pivot_wider(id_cols = Time, names_from = Model, values_from = f) %>% 
        stats::na.omit() %>% 
        dplyr::select(-Time) %>% 
        private$add_cuts(private$get_cuts())
      
      print(str(forecasts))
      
      if (naive == TRUE){
        if (comb == 'mean'){
          forecasts = forecasts %>% 
            dplyr::mutate(ensemble = rowMeans(.))
        }
        if (comb == 'median'){
          forecasts = forecasts %>% 
            dplyr::mutate(ensemble = Rfast::rowMedians(as.matrix(.)))
        }
      }
      else{
        if (private$get_ensemble_built() == FALSE){
          warning("The ensemble with a glm model has not been built yet. A simple glm ensemble will be built. If you need more granularity, please use the 'build_ensemble' method to create it manually.")
          self$create_ensemble()
        }
        
        forecasts = forecasts %>% 
          dplyr::mutate(ensemble = stats::predict(private$get_ensemble_model(), newdata = forecasts)) %>% 
          dplyr::mutate_if(is.numeric, as.double)  # Converts Named numeric (output of predict) to simple numeric 
        
      }
      
      return(forecasts)
      
    }
  ),
  
  
  #### Private Methods ----
  private = list(
    data = NA,
    var_interest = NA,
    uni_models = NA,
    var_models = NA,
    mlp_models = NA,
    verbose = NA,
    ensemble_model = NA,
    ensemble_built = NA,
    cuts = NA,
    
    set_data = function(data){
      if (all(is.na(data))){ stop("You have not provided the time series data. Please provide to continue.") }
      private$data = data
    },
    
    set_var_interest = function(var_interest){private$var_interest = var_interest},
    
    get_verbose = function(){return(private$verbose)},
    
    get_data_subset = function(col_names){return(self$get_data() %>% dplyr::select(col_names))},
    
    set_uni_compare_objects = function(models){
      private$uni_models = c(models) # coerce to list
    },
    get_uni_compare_objects = function(){return(private$uni_models)},
    
    set_var_compare_objects = function(models){
      private$var_models = c(models) # coerce to list
    },
    get_var_compare_objects = function(){return(private$var_models)},
    
    set_mlp_compare_objects = function(models){
      private$mlp_models = c(models) # coerce to list
    },
    get_mlp_compare_objects = function(){return(private$mlp_models)},
    
    set_ensemble_model = function(model){
      private$ensemble_model = model
    },
    get_ensemble_model = function(){return(private$ensemble_model)},
    
    get_len_x = function(){return(nrow(self$get_data()))},
    
    set_ensemble_built = function(ar) { private$ensemble_built = ar},
    get_ensemble_built = function() { return(private$ensemble_built)},
    
    set_cuts = function(ar) { private$cuts = ar},
    get_cuts = function() { return(private$cuts)},
    
    filter_best_caret_model = function(data, caret_compare_object){
      # Given a caret_compare_object and a dataframe 'data' that has a 'Model' column
      # indicating the caret model ID, this method will filter out the data to include 
      # the results from only the best (final) model
      
      if (!("ModelCompareNNforCaret" %in% class(caret_compare_object))){
        warning("You have not passed a compare object of type ModelCompareNNforCaret. Results will not be filtered.")
      }
      else{
        best_model_id = caret_compare_object$get_best_model_id()
        
        data = data %>% 
          assertr::verify(assertr::has_all_names("Model")) %>% 
          dplyr::filter(Model == best_model_id)
      }
      
      return(data)
    
    },
    
    validate_models = function(){
      # TODO: Check if there is at least one object with at least 1 sliding ASE model
      # Is this needed? What if we just need to compare and not build an ensemble
    },
    
    any_sliding_ase = function(){
      ## This seems like a hack. Had to do this since any_sliding_ase is a private method in the compare objects
      ## TODO: evaluate if this needs to be changed to a public method
      
      any_sliding = FALSE
      
      ASEs = data.frame()
      
      uni_compare_objects = private$get_uni_compare_objects()
      for (i in seq_along(uni_compare_objects)){
        subset_results = uni_compare_objects[[i]]$plot_batch_ases(only_sliding = TRUE, plot = FALSE, silent = TRUE)
        ASEs = dplyr::bind_rows(ASEs, subset_results$ASEs)
      }
      
      var_compare_objects = private$get_var_compare_objects()
      for (i in seq_along(var_compare_objects)){
        subset_results = var_compare_objects[[i]]$plot_batch_ases(only_sliding = TRUE, plot = FALSE, silent = TRUE)
        ASEs = dplyr::bind_rows(ASEs, subset_results$ASEs)
      }
      
      mlp_compare_objects = private$get_mlp_compare_objects()
      for (i in seq_along(mlp_compare_objects)){
        subset_results = mlp_compare_objects[[i]]$plot_batch_ases(only_sliding = TRUE, plot = FALSE, silent = TRUE) 
        
        filtered = subset_results$ASEs %>%  
          private$filter_best_caret_model(caret_compare_object = mlp_compare_objects[[i]])
        
        ASEs = dplyr::bind_rows(ASEs, filtered)
      }
      
      if (nrow(ASEs) >= 1){
        any_sliding = TRUE
      }
      
      return(any_sliding)
    },
    
    add_cuts = function(data, cuts = NA){
      
      # if (!all(is.na(cuts))){
      #   # minValue = min(apply(data,2,min))
      #   # maxValue = max(apply(data,2,max))
      #   
      #   cuts = c(-1E9, cuts, 1E9)
      #   
      #   # print(paste0("Min: ", minValue))
      #   # print(paste0("Max: ", minValue))
      #   
      #   # if (minValue < cuts[1]){
      #   #   warning("The cuts do not cover the minimum value of the predictons. Hence the cuts will be adjusted.")
      #   # cuts = c(-1E9, cuts)
      #   # }
      #   # if (maxValue > cuts[length(cuts)]){
      #   #   warning("The cuts do not cover the maximum value of the predictons. Hence the cuts will be adjusted.")
      #   #  cuts = c(cuts, 1E9)
      #   # }
      #   
      #   print(cuts)
      #   
      #   cut_data = as.data.frame(apply(data, 2, function(x) {cut(as.numeric(x), cuts)}))
      #   
      #   if (any(is.na(cut_data))){
      #     warning("Making cuts introduced NA values. Please make sure that the cut values are sufficiently large to cover the min and max range of the predictions.")
      #   }
      #   
      #   colnames(cut_data) = paste0(colnames(cut_data), "_cut")
      #   data = data %>% bind_cols(cut_data)
      # }
      
      warning("Cuts is not supported curently. This may be added at a later point in time.")
      
      return(data)
    }
    
    
    
    
    
    
  )
  
)

