szn_trans <- function(x,n){
	artrans.wge(x, phi.tr = c(rep(0, n - 1), 1))
}


arima_trans <- function(x, n) {
    f <- artrans.wge(x, phi.tr = 1)
    if (n == 1) {
        f
    } else {
        arima_trans(f, n - 1)
    }
}


#' Time Series transformation 
#' @param type a string or just plain text, either arima or aruma/seasonal
#' @param x a time series
#' @param n the depth to transform the time series
#' @examples
#' ts <- generate(arima, n = 100, d = 2)
#' transform(arima, ts,2)
#' ts2 <- generate(aruma, n = 100, s =12)
#' transform("seasonal", ts2, n = 12)

transform <- function(type, x, n){
	if(is.character(enexpr(type)) == F){
		type <- as.character(enexpr(type))
	}
	if (type %in% c('arima',"ARIMA","Arima")){
		arima_trans(x,n)
	}
	if (type %in% c('ARUMA','Aruma','aruma','Seasonal','seasonal')){
		szn_trans(x,n)
	}
	else {
		print("enter a valid type of transformation! \n Valid types include: \n ARIMA/arima/Arima/seasonal/Seasonal/Aruma/ARUMA")
	}
}
