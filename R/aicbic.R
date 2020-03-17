aics <- list(
             aic <- function(x, p = 0:8, q = 0:5, silent) aic5(x, p, q, type = "aic", silent),
             bic <- function(x, p = 0:8, q = 0:5, silent) aic5(x, p, q, type = "bic", silent)
)

#' aicbic function
#' returns the top 5 aic and bic values for an ARMA time series
#' @param vec the vector (time series object) to operate on
#' @param p integer vector of the ar order
#' @param q integer vector of the ma order
#' @param parallel whether or not to run in parallel
#' @param cl the cluster in which to run on
#' @param silent whether or not to run silently
#' @param merge If TRUE, the AIC and BIC grids are merged so that it is 
#'              easier to see which models are common between the two
#' @param sort_by If merge = TRUE, then this is used to sort the merged dataframe
#'                valid options are 'aic' or 'bic'
#' @return a list of data frames of top 5 aic and bic
#' @export
#' @examples
#' xs <- playground(200)
#' aicbic(xs)
aicbic <- function(vec, p = 0:8, q = 0:5, parallel = FALSE, cl = NULL, silent = FALSE, merge = FALSE, sort_by = "aic"){
  
  if(parallel == TRUE){
    rvGrid = parallel::parLapply(cl, aics, function(f) f(vec, p, q, silent = TRUE))
  } else {
    rvGrid = lapply(aics, function(f) f(vec, p, q, silent))
  }
  
  if (merge == FALSE){
    return(rvGrid)
  }
  else{
    aic_vals = rvGrid[[1]]
    bic_vals = rvGrid[[2]]
    
    rvGrid = dplyr::full_join(aic_vals, bic_vals, by = c("p", 'q')) %>% 
      dplyr::arrange_at(sort_by)
    
    return(rvGrid)
  }
}
