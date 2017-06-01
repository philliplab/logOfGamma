#' Computes the natural logarithm of the gamma function for values larger than 12.
#'
#' Uses the approximation in Hart et al, Computer Approximations 1968.
#'
#' @param x A numeric value of length 1 greater than 12
#' @examples
#' gammaln_internal(50)
#' @export

gammaln_internal <- function(x){
  stopifnot(length(x) == 1)
  stopifnot(x > 12)
  a <- c(-1.910444077728e-03, 
         8.4171387781295e-04, 
         -5.952379913043012e-04, 
         7.93650793500350248e-04, 
         -2.777777777777681622553e-03, 
         8.333333333333333331554247e-02, 
         5.7083835261e-03)
  spi <- 0.9189385332046727417803297
  x_sq <- x*x
  result <- a[7]
  for (i in 1:6){
    result <- result / x_sq + a[i]
  }
  result <- result / x
  result <- result + spi - 0.5*log(x) + x * (log(x) -1)
  return(result)
}

#' Computes the natural logarithm of the gamma function.
#'
#' For values larger than 12, an approximation is used.
#'
#' @param x A numeric vector of positive numbers.
#' @examples
#' gammaln(5)
#' gammaln(50)
#' @export

gammaln <- function(x){
  y <- x
  for (i in 1:length(x)){
    if (x[i] > 12) {
      y[i] <- gammaln_internal(x[i])
    } else {
      y[i] <- log(gamma(x[i]))
    }
  }
  return(y)
}

