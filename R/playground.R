types <- c(rep("arima",15), rep("arma", 10), rep("aruma",15), rep("sigplusnoise", 3) )

# because i dont understand signal plus noise well, weights can be adjusted

# random type of time series
rtype <- function() sample(types,1)

# random seasonal component
rszn <- function() floor(runif(1,0,25))

# random integrated component (goes from 0 to 3)
rint <- function() floor(runif(1,0,4))

# random p order (could be improved)
rphilen <- function() floor(runif(1,0.99,20))

# random q order
rthetalen <- function() floor(runif(1,0,5))

# check if ar component is stationary (stolen from the R stats source code)
phicheck <- function(phi){
  min(Mod(polyroot(c(1,-phi))))
}

# recursively defined random phis, lots of room for improvement
# if it is nonzero length, return a stationary vector of phis
rphi <- function(){
  len <- rphilen()
  phis <- rnorm(len, 0,1)
  if(length(phis > 0)){
    if (phicheck(phis) <= 1){
      rphi()
    }
    else {
      return(phis)
    }

  }
  else return(0)
}

# random thetat vector
rtheta <- function(){
  len <- rthetalen()
  thetas <- runif(len, -1.5, 1.5)
  if(length(thetas) >0) {
    return(thetas)
  }
  else {
    return(0)
  }
}

# random sigplusnoise component
rb0 <- function() runif(1, -100,100)
rb1 <- function() runif(1, -2, 2)
rfreq <- function() runif(2, 0, .5)
rpsi <- function() runif(2, 0, 2*pi)
rwnv <- function() abs(rnorm(1,1,1))
rcof <- function() runif(2, -2, 2)

# random sognal plud noise
rsigpn <- function(n, p) {
  tswge::gen.sigplusnoise.wge( n,
                       b0 = rb0(),
                       b1 = rb1(),
                       freq = rfreq(),
                       psi = rpsi(),
                       vara = rwnv(),
                       coef = rcof(), plot = p)
}

# random arma
rarma <- function(n, p) {
  tswge::gen.arma.wge( n,
               phi = rphi(),
               theta = rtheta(), plot = p)
}

# radnom arima
rarima <- function(n, p) {
  tswge::gen.arima.wge( n,
                phi = rphi(),
                theta = rtheta(),
                d = rint(), plot = p)
}

# random aruma
raruma <- function(n, p) {
  tswge::gen.aruma.wge( n,
                phi = rphi(),
                theta = rtheta(),
                d = rint(),
                s = rszn(), plot = p)
}

#' Playground: generate a random time series for practice
#' @param n the length of the resulting time series
#' @return a random time series, TODO add weights
#' @export
playground <- function(n, plot = TRUE) {
  type <- rtype()
  switch(type,
         "arima" = return(rarima(n, p = plot)),
         "aruma" = return(raruma(n, p = plot)),
         "arma" = return(rarma(n, p = plot)),
         "sigplusnoise" = return(rsigpn(n, p = plot))
  )
}
