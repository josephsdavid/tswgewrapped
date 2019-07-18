
#' Time Series transformation 
#' @param type either aruma/seasonal or arima, type of transformation to make
#' @param x the time series
#' @param n the order of transformation
#' @return the transformed time series
#' @export
difference <- function(type, x, n){
	szn_trans <- function(x,n){
		artrans.wge(x, phi.tr = c(rep(0, n - 1), 1))
	}
	arima_trans <- function(x, n) {
	    f <- artrans.wge(x, phi.tr = 1)
	    if (n == 1) {
	        res <- f
	    	return(res)
	    } else {
	        arima_trans(f, n - 1)
	    }
	}
	if(is.character(enexpr(type)) == F){
		type <- as.character(enexpr(type))
	}
	if (type %in% c('arima',"ARIMA","Arima")){
		return((arima_trans(x,n)))
	}
	if (type %in% c('ARUMA','Aruma','aruma','Seasonal','seasonal')){
		szn_trans(x,n)
	}
}
