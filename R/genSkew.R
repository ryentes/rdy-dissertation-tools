#' Simulates a skewed careless respondent
#' 
#' Given a vector of survey responses, an insert, and a number of items,
#' this function simulates a skewed careless respondent.
#' 
#' @keywords confusion matrix
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param x the length of the span of careless responses
#' @param mu the mean of the distribution from from which to draw the skewed respondent
#' @param sigma the standard deviation of the distribution from which to draw the skewed respondent
#' @param floor the lowest possible value for a response
#' @param the highest possible value for a response
#' @export
#' 

genSkew <- function(x,mu=5.75, sigma=1.25, floor=1,ceiling=7) {
  r <- round(stats::rnorm(x, mu, sigma))
  r[r>ceiling] <- ceiling
  r[r<floor] <- floor
  return(r)
}