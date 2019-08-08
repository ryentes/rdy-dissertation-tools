#' A function to calculate the CI of informedness
#' 
#' This function takes a point estimate of informedness, a simulation size,
#' and a level of confidence as parameters.
#' 
#' @keywords confidence interval
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param b the point estimate for informedness
#' @param n the number of observations in the sample
#' @param alpha the significance 
#' @export
informCI <- function(b, n, alpha=.05) {
  sse_b <- (1-b)^2
  se <- sqrt((sse_b/(n-1)))
  conf = (1-alpha) + (alpha/2)
  z = qnorm(conf)
  
  upperCI = b + (z*se)
  lowerCI = b - (z*se)
  
  return(c(lowerCI, upperCI))
}
