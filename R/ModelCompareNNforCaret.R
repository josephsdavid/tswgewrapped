#' @title R6 class ModelCompareMultivariate
#' @export
ModelCompareNNforCaret = R6::R6Class(
  classname = "ModelCompareNNforCaret",
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
    #' @param mdl_list A single caret model (which may contain results of grid or random search)
    #' @param verbose How much to print during the model building and other processes (Default = 0)
    #' @return A new `ModelCompareNNforCaret` object.
    initialize = function(data = NA, var_interest = NA, mdl_list, verbose = 0)
    {
      if (class(mdl_list) == 'mlp'){
        stop("You have passed the 'mlp' model to this class. You need to pass the caret model. This can be obtained from the ModelBuildNNforCaret class object by using the get_final_models method with subset = 'a' argument.")
      }
      
      if (!(any(class(mdl_list) == 'train') & any(class(mdl_list) == 'train.formula'))){
        stop("You have not passed a caret model to this class. You need to pass the caret model. This can be obtained from the ModelBuildNNforCaret class object by using the get_final_models method with subset = 'a' argument.")
      }
      
      private$set_caret_model(caret_model = mdl_list)
      n.ahead = private$compute_n.ahead()
      batch_size = private$compute_batch_size()
      private$set_var_interest(var_interest = var_interest)
      super$initialize(data = data, mdl_list = mdl_list,
                       n.ahead = n.ahead, batch_size = batch_size, step_n.ahead = TRUE,
                       verbose = verbose)
      
    },
    
    #### Getters and Setters ----
    
    #' @description Returns the dependent variable name
    #' @return The dependent variable name
    get_var_interest = function(){return(private$var_interest)},
    
    #' @description Returns the dependent variable data only
    #' @return The dependent variable data only
    get_data_var_interest = function(){return(self$get_data()[, self$get_var_interest()])},
    
    #' @description Returns the model ID for the best model from the search
    #' @return The model ID for the best model from the search
    get_best_model_id = function(){
      return(private$get_caret_model()[['bestTune']] %>%
               private$add_model_id() %>% 
               purrr::pluck("ID"))
    },
    
    #### General Public Methods ----
   
    #' @description Not applicable for the nnfor::mlp models, since we are passing already build models 
    summarize_build = function(){
      
    }
    
  ),
  
  
  #### Private Methods ----
  private = list(
    var_interest = NA,
    caret_model = NA,
    
    set_var_interest = function(var_interest){private$var_interest = var_interest},
    
    get_data_subset = function(col_names){
      return(self$get_data() %>% dplyr::select(col_names))
    },
    
    set_caret_model = function(caret_model){private$caret_model = caret_model},
    
    get_caret_model = function(){
      return(private$caret_model)
    },
    
    get_len_x = function(){
      return(nrow(self$get_data()))
    },
    
    clean_model_input = function(mdl_list, batch_size){
      ## mdl_list is actually the caret model here (which technically is also a list :))
     
      results_with_id = mdl_list[['results']] %>% 
        private$add_model_id()
      
      sliding_ase = ifelse(nrow(mdl_list[['pred']] %>% dplyr::select(Resample) %>% unique()) > 1, TRUE, FALSE)

      rv_mdl_list = list()
      
      for (i in seq_len(nrow(mdl_list[['results']]))){
        subset = results_with_id %>% dplyr::slice(i)
        
        name = subset %>% purrr::pluck("ID")
        
        rv_mdl_list[[name]][['reps']] = subset %>% purrr::pluck("reps")
        rv_mdl_list[[name]][['hd']] = subset %>% purrr::pluck("hd")
        rv_mdl_list[[name]][['allow.det.season']] = subset %>% purrr::pluck("allow.det.season")
        rv_mdl_list[[name]][['sliding_ase']] = sliding_ase
        rv_mdl_list[[name]][['metric_has_been_computed']] = FALSE
        
      }
      
      return(rv_mdl_list)
    },
    
    get_sliding_ase_results = function(name, step_n.ahead){
      
      ase_data = private$clean_resample_info(data = private$get_caret_model()[['resample']], subset = name) %>% 
        dplyr::mutate(time_test_start = Resample + 1) %>% 
        dplyr::mutate(time_test_end = Resample + self$get_n.ahead()) %>% 
        dplyr::mutate(batch_num = (Resample + self$get_n.ahead() - self$get_batch_size())/self$get_n.ahead() + 1)

      pred_data = private$clean_resample_info(data = private$get_caret_model()[['pred']], subset = name)

      res = list()
      res$ASEs = ase_data %>% purrr::pluck("ASE")
      res$time_test_start = ase_data %>% purrr::pluck("time_test_start")
      res$time_test_end = ase_data %>% purrr::pluck("time_test_end")
      res$batch_num = ase_data %>% purrr::pluck("batch_num")
      res$f = pred_data %>% purrr::pluck("pred")
      res$ll = pred_data %>% purrr::pluck("pred")
      res$ul = pred_data %>% purrr::pluck("pred")
      res$time.forecasts = pred_data %>% purrr::pluck("rowIndex")
      
      return (res)
    },
    
    compute_simple_forecasts = function(lastn, newxreg){
      if (lastn == TRUE){
        message(paste0("The '",  self$classname, "' class does not support lastn = TRUE since the model has already been built using the entire data. Hence, lastn will be set to FALSE."))
        lastn = FALSE
      }
      
      if (all(is.na(newxreg))){
        stop("You have not provided the values for 'newxreg' which are needed to make future forecasts")
      }
      
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

      ## nnfor::mlp needs xreg to be a dataframe
      newxreg = as.data.frame(newxreg)
      
      from = data_end + 1
      to = data_end + nrow(newxreg) # self$get_n.ahead()
      
      original_xreg = private$get_caret_model()[['trainingData']] %>%
        dplyr::select(- .outcome)

      reqd_cols = colnames(original_xreg)
      
      if (!all(sort(reqd_cols) == sort(colnames(newxreg)))){
        stop("The colnames for newxreg dont match those provided during the training of the model.")
      }
      
      newxreg = newxreg %>%
        # assertr::verify(assertr::has_all_names(!!reqd_cols)) %>%  ## does not accept variables
        dplyr::select(reqd_cols)
      
      ## caret predict (so we only need to pass newxreg)
      #full_xreg = dplyr::bind_rows(original_xreg, newxreg)
      forecasts = caret::predict.train(private$get_caret_model(), newdata = newxreg)  
      
      results = results %>% 
        dplyr::add_row(Model = self$get_best_model_id(),
                       Time = (from:to),
                       f = forecasts$mean,
                       ll = forecasts$mean,
                       ul = forecasts$mean)
      
      return(results)
    },
    
    add_model_id = function(dataframe){
      ## For a dataframe containing the columns reps, hd, and allow.det.season, 
      ## this will add the model ID to each row
      return(
        dataframe %>%
          assertr::verify(assertr::has_all_names("reps", "hd", "allow.det.season")) %>% 
          dplyr::mutate(ID = paste0("reps", reps, "_hd", hd, "_sdet", allow.det.season))
        )
      
    },
    
    compute_batch_size = function(){
      data = private$get_data_to_compute_batch_info()
      # Batch Size Definition = Training + Test 
      batch_size = data %>% purrr::pluck("Resample") %>% min() + private$compute_n.ahead()
      
      # if (private$get_verbose() >= 1){
      #   print(paste0("Batch Size: ", batch_size))
      # }
      return(batch_size)
    },
    
    compute_n.ahead = function(){
      data = private$get_data_to_compute_batch_info()
      #n.ahead = nrow(private$get_caret_model()[['trainingData']]) - data %>% purrr::pluck("Resample") %>% max()
      temp = data %>% purrr::pluck("Resample")
      
      ## TODO: Add check if there is only 1 resample, then we need to somehow take care of it.
      n.ahead = temp[2] - temp[1]
      
      # if (private$get_verbose() >= 1){
      #   print(paste0("n.ahead: ", n.ahead))
      # }
      return(n.ahead)
    },
    
    get_data_to_compute_batch_info = function(){
      data = private$get_caret_model()[['resample']] %>% 
        private$add_model_id()
      
      one_name = data %>% dplyr::slice(1) %>% purrr::pluck("ID")
      data = private$clean_resample_info(data = private$get_caret_model()[['resample']], subset = one_name)
      
      return(data)
    },
    
    clean_resample_info = function(data, subset = NA){
      # data is a dataframe with columns 'reps', 'hd', 'allow.det.season' and 'Resample'
      # subset = ID/name of the model (single) to get; NA will get all
      data = data %>% 
        assertr::verify(assertr::has_all_names("reps", "hd", "allow.det.season", "Resample")) %>% 
        private$add_model_id() %>% 
        dplyr::mutate(subset = ifelse(is.na(subset), ID, subset)) %>%
        dplyr::filter(ID == subset) %>% 
        dplyr::mutate(Resample = as.numeric(gsub("Training", "", Resample))) %>% 
        dplyr::arrange(Resample)
      
      return(data)
    }
    
  )
  
)

