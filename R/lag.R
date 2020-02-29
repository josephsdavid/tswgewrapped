#' Prevents inter 'cat' statememt from printing
#' @param f Function to silence
#' @export
#' @examples
#' hush(ljung_box(1:100, 2,4))
hush <- function(f) {
  utils::capture.output(res <- f)
  return(res)
}

#' lag_dfr: Lag columns of a data frame
#' @param df The dataframe containing the time series data
#' @param col_name The column name (single) to be used to create the lagged variables
#' @param lags Number of lags to create for the column
#' @export
#' @examples
#' x<-  c(2,3,5,7,9,9,5,3)
#' z <- c(6,9,3,3,3,4,5,7)
#' y <- 1:8
#' df <- data.frame(Y = y, X = x, Z =z)
#' lag_dfr(df, "X", 1:3)
lag_dfr <-function(df, col_name, lags){
  out <- lapply(lags, function(x) stats::lag(df[[col_name]], x))
  out <- do.call(cbind.data.frame,out)
  cnames <- sapply(lags, function(x) paste(col_name, x, sep = "_"))
  colnames(out) <- cnames
  out
}

#' mlag_dfr: Lag multiple columns of a data frame
#' @param df The dataframe containing the time series data
#' @param col_names A vector of column names to be used to create the lagged variables
#' @param lags Number of lags to create for the column
#' @export
#' @examples
#' x  <-  c(2,3,5,7,9,9,5,3)
#' z <- c(6,9,3,3,3,4,5,7)
#' y <- 1:8
#' df <- data.frame(Y = y, X = x, Z =z)
#' lag_dfr(df, "X", 1:3)
#' mlag_dfr(df, c("X","Z"), 1:3)
mlag_dfr <- function(df, col_names, lags){
  out <- lapply(col_names, function(x) lag_dfr(df, x, lags))
  out <- do.call(data.frame, unlist(out, recursive = FALSE))
  cbind(df,out)
}

