#' @title R6 class ModelBuildNNforCaret
#' 
#' @export
ModelBuildNNforCaret = R6::R6Class(
  classname = "ModelBuildNNforCaret",
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
    #' @param m The frequency of the variable of interest
    #' @param search Caret grid search method: 'grid' or 'random' (Default = 'grid')
    #' @param grid If search = 'grid', what combinations of hyperparameters to use (See format in vignette).
    #'             Allowed parameters in grid are "reps", "hd", and "allow.det.season"
    #' @param tuneLength If search = 'random', how many random combinations to try (Default = 3)
    #' @param batch_size Batch Size to use 
    #' @param h Forecast Horizon 
    #' @param parallel Should the grid search be run in parallel or not (Default = TRUE)
    #' @param seed The seed to use for training the the Neural Network (Default = 1)
    #' @param verbose How much to print during the model building and other processes (Default = 0)
    #' @param ... Additional parameers to feed to nnfor::mlp function for building the model
    #'            It is highly recommended to pass the frequency of the variable of interest 'm' to get a good model
    #'            Other arguments that can be passed can be found by typing ?nnfor::mlp in the console
    #' @return A new `ModelBuildNNforCaret` object.
    initialize = function(data = NA, var_interest = NA, m = NA,
                          search = 'grid', 
                          grid = NA, tuneLength = NA,
                          batch_size = NA, h = NA,
                          parallel = TRUE, 
                          seed = 1,
                          verbose = 0, ...)
    {
      # https://stackoverflow.com/questions/34520448/source-file-in-r-package
      # source("R/source_caret_nnfor.R")
      
      private$set_data(data = data)
      private$set_var_interest(var_interest = var_interest)
      private$set_m(m = m)
      
      private$set_search(search = search)
      private$set_grid(grid = grid)
      private$set_tune_length(tune_length = tuneLength)
      private$set_h(h = h)
      private$set_initial_window(batch_size = batch_size, h = private$get_h())  
      private$set_parallel(parallel = parallel)
      private$set_seed(seed = seed)
      private$set_verbose(verbose = verbose)
      
      private$build_models(...)
      
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the time series realization
    #' @return The Time Series Realization
    get_data = function(){return(private$data)},
    
    
    
    # #' @description Returns the dependent variable data only
    # #' @return The dependent variable data only
    # get_data_var_interest = function(){return(self$get_data()[, private$get_var_interest()])},

    #### General Public Methods ----
   
    #' @description Summarizes the results of all the hyperparameter combinations
    #' @returns A dataframe containing the information about the different models
    summarize_hyperparam_results = function(){
      caret_model = self$get_final_models(subset = 'a')
      return(caret_model$results)
    },
    
    #' @description Summarizes the best hyperparameter combination
    #' @returns A dataframe containing the hyperparameters for the best model
    summarize_best_hyperparams = function(){
      caret_model = self$get_final_models(subset = 'a')
      return(caret_model$bestTune)
    },
    
    #' @description Plots the ASE metric variation along the hyperparameter space
    #' @param level_plot A boolean indicating whether a level plot should be shown. useful for 'grid' search (Default = TRUE).  
    plot_hyperparam_results = function(level_plot = TRUE){
      caret_model = self$get_final_models(subset = 'a')
      
      print(ggplot2::ggplot(caret_model))

      if (level_plot == TRUE){
        # Opt 3 (Useful for grid searches not for random)
        # lattice::trellis.par.set(caretTheme())
        plot(caret_model, metric = "ASE", plotType = "level",
             scales = list(x = list(rot = 90)))
      }
    },
    
    #' @description Returns a final models 
    #' @param subset The subset of models to get.
    #'              'a': All models (Default)
    #'              'r': Only the recommended models
    #' @return If subset = 'a', returns the caret model object
    #'         If subset = 'r', returns just the nnfor model
    get_final_models = function(subset = 'a'){
      if (subset != 'a' & subset != 'r'){
        warning("The subset value mentioned is not correct. Allowed values are 'a', or 'r. The default value 'a' will be used")
        subset = 'a'
      }
      
      if (subset == 'a'){
        return(private$get_models())
      }
      else if (subset == 'r'){
        return(private$get_models()$finalModel)
      }
      
    },
    
    #' @description Summarizes the entire build process
    #' @param level_plot A boolean indicating whether a level plot should be shown. useful for 'grid' search (Default = TRUE).  
    summarize_build = function(level_plot = TRUE){
      cat("\n\n------------------------------")
      cat("\nHyperparameter Tuning Results:")
      cat("\n------------------------------\n\n")
      print(self$summarize_hyperparam_results())
      
      self$plot_hyperparam_results(level_plot = level_plot)
      
      cat("\n\n---------------------")
      cat("\nBest Hyperparameters:")
      cat("\n---------------------\n\n")
      print(self$summarize_best_hyperparams())
      
      cat("\n\n--------------")
      cat("\nFinal Model:")
      cat("\n--------------\n\n")
      print(self$get_final_models(subset = 'r'))
    }
    
  ),
  
  #### Private Methods ----
  private = list(
    data = NA,
    var_interest = NA,
    m = NA,
    models = NA,
    search = NA,
    grid = NA,
    tuneLength = NA,
    initialWindow = NA,
    h = NA,
    parallel = TRUE, 
    seed = 1,
    verbose = NA,
    
    set_data = function(data){
      if (all(is.na(data))){ stop("You have not provided the time series data. Please provide to continue.") }
      private$data = data
    },
    
    get_selected_data = function(col_names){
      return(self$get_data() %>% dplyr::select(col_names))
    },
    
    set_var_interest = function(var_interest){private$var_interest = var_interest},
    
    get_var_interest = function(){return(private$var_interest)},
    
    set_m = function(m){
      if (is.na(m)){
        warning("You have not specified the frequency of the variable of interest (univariate frequency in nnfor::mlp). This will be set to 1, but the models built may not be very good. It is highly recommened that you pass the appropriate frquency to this object")
        private$m = 1
      }
      else{
        private$m = m  
      }
    },
    
    get_m = function(){return(private$m)},
    
    set_models = function(models){private$models = models},
    
    get_models = function(){return(private$models)},
    
    set_search = function(search = "grid"){
      if (search != "grid" & search != "random"){
        warning("You have not specified the correct value for the 'search' argument. Allowed values are 'grid' or 'random. This will be set to 'grid' by default.")
        private$search = "grid"
      }
      else{
        private$search = search
      }
    },
    
    get_search = function(){return(private$search)},
    
    set_grid = function(grid){
      if (all(is.na(grid))){
        private$grid = nnfor_caret$grid()
      }
      else{
        private$grid = grid
      }
    },
    
    get_grid = function(){return(private$grid)},
    
    set_tune_length = function(tune_length){
      if (is.na(tune_length)){
        private$tune_length = 3
      }
      else{
        private$tune_length = tune_length
      }
    },
    
    get_tune_length = function(){return(private$tune_length)},
    
    set_h = function(h){
      if (is.na(h)){
        stop("You have not specified the forecast horizon 'h'. Please specify horizon to proceed.")
      }
      
      private$h = h
    },
    
    get_h = function(){return(private$h)},
    
    set_initial_window = function(batch_size, h){
      if (is.na(batch_size)){
        message("You have not specified the 'batch_size', so only 1 batch will be used.")
        batch_size = nrow(self$get_data())
      }
      
      private$initialWindow = batch_size - h
    },
    
    get_initial_window = function(){return(private$initialWindow)},
    
    set_parallel = function(parallel = TRUE){
      private$parallel = parallel
    },
    
    get_parallel = function(){return(private$parallel)},
    
    set_seed = function(seed = 1){
      private$seed = seed
    },
    
    get_seed = function(){return(private$seed)},
    
    set_verbose = function(verbose = 0){
      private$verbose = verbose
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
    
    build_models  = function(...){
      
      fitControl = private$get_fit_control(initialWindow = private$get_initial_window(),
                                           h = private$get_h(),
                                           search = private$get_search(),
                                           verbose = as.logical(private$get_verbose()),
                                           parallel = private$get_parallel())
      
      # print(fitControl)
      
      # http://sshaikh.org/2015/05/06/parallelize-machine-learning-in-r-with-multi-core-cpus/
      if (private$get_parallel() == TRUE){
        
        # https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
        # https://stackoverflow.com/questions/41307178/error-processing-vignette-failed-with-diagnostics-4-simultaneous-processes-spa
        chk = Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
        
        if (nzchar(chk) && chk == "TRUE") {
          # use 2 cores in CRAN/Travis/AppVeyor
          num_workers = 2L
        } else {
          # use all cores in devtools::test()
          num_cores = parallel::detectCores()
          num_workers = ifelse(num_cores <= 1, 1, num_cores - 1) # Leave 1 out
        }
        
        cl = parallel::makeCluster(num_workers) 
        
        doParallel::registerDoParallel(cl)
      }
      
      form = as.formula(paste(private$get_var_interest(), ".", sep=" ~ "))
      
      # print(paste0("Formula: ", form))
      # print("Grid: ")
      # print(private$get_grid())
      # print(paste0("Tune Length: ", private$get_tune_length()))
      # print(paste0("Frequency: ", private$get_m()))
      
      tictoc::tic("- Total Time for training: ")
      
      set.seed(private$get_seed())
      # Must pass a dataframe to caret::train, hence we need to specify the frequency of y separately using 'm'
      
      # https://stackoverflow.com/questions/42180071/error-in-model-frame-default-variable-lengths-differ-linear-model-validation
      # Dont use private$get_var_interest() inside formula since it has '$' sign. It causes issues
      
      if (private$get_search() == "grid"){
        nnfor_model = caret::train(form,
                                   data = self$get_data(),
                                   method = nnfor_caret,
                                   trControl = fitControl,
                                   tuneGrid = private$get_grid(),
                                   metric = "ASE", maximize = FALSE,
                                   ## Additional arguments to the nnfor::mlp function
                                   m = private$get_m(),
                                   ...)
      }
      else if (private$get_search() == "random"){
        nnfor_model = caret::train(form,
                                   data = self$get_data(),
                                   method = nnfor_caret,
                                   trControl = fitControl,
                                   tuneLength = private$get_tune_length(),
                                   metric = "ASE", maximize = FALSE,
                                   ## Additional arguments to the nnfor::mlp function
                                   m = private$get_m(),
                                   ...)
      }

      tictoc::toc()
      
      if(private$get_parallel() == TRUE){
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
      }
      
      private$set_models(models = nnfor_model)
    },
    
    # In order to make models fully reproducible when using parallel processing, we need to pass seeds as a parameter
    # https://stackoverflow.com/questions/13403427/fully-reproducible-parallel-models-using-caret
    get_seeds_for_caret = function(search, fitControl){
      if (private$get_search() == "grid"){
        total.param.permutations = nrow(private$get_grid())
      }
      else if (private$get_search() == "random"){
        total.param.permutations = private$get_tune_length()
      }
      
      folds = (nrow(self$get_data()) - fitControl$initialWindow)/fitControl$horizon  ## change to integer
      
      # length is = (n_repeats*nresampling)+1
      seeds = vector(mode = "list", length = folds + 1)
      
      # set.seed(1)  
      
      for(i in 1:folds) seeds[[i]] = sample.int(n = private$get_seed(),
                                                total.param.permutations,
                                                replace = TRUE)
      # for the last model
      seeds[[folds + 1]] = sample.int(private$get_seed(), 1, replace = TRUE)
      
      return(seeds)
    },
    
    get_fit_control = function(initialWindow, h, search = "random", verbose = TRUE, parallel = TRUE){
      
      # print(paste0("get_fit_control >> verbose: ", verbose))
      
      fitControl = caret::trainControl(method = "timeslice",
                                       horizon = h,
                                       skip = h-1,
                                       fixedWindow = TRUE,
                                       summaryFunction = caret_metric_ASE,
                                       verboseIter = verbose,
                                       returnData = TRUE,
                                       returnResamp = "all",
                                       savePredictions = TRUE,
                                       allowParallel = parallel)
      
      fitControl$initialWindow = initialWindow
      fitControl$search = search
      fitControl$horizon = h
      fitControl$skip = h-1
      
      fitControl$seeds = private$get_seeds_for_caret(search = search, fitControl = fitControl)
      
      return(fitControl)
    }
    
    
    
    
  )
  
)

