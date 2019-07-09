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
#' @export
tstransform <- function(type, x, n){
	if(is.character(enexpr(type)) == F){
		type <- as.character(enexpr(type))
	}
	if (type %in% c('arima',"ARIMA","Arima")){
		arima_trans(x,n)
	}
	if (type %in% c('ARUMA','Aruma','aruma','Seasonal','seasonal')){
		szn_trans(x,n)
	}
}
