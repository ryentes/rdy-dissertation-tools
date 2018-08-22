#' A careless responding model object
#' 
#' This function defines a model of careless responder. Objects using this class
#' are initialized with the following parameters
#' 
#' @keywords data model
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param mu the hypothetical mean occurance in a sample
#' @param sigma the hypothetical standard deviation of occurance in a sample
#' @param n the total number of respnodents in the proposed sample
#' @export
getProportionFromModel <- function(mu, sigma, n) {
  x <- 0
  
  while (x < 1) {
    x <- round(abs(rnorm(1, mu,sigma)*n))
  }
  
  return(x)
}
