#' Simulates a centered careless respondent
#' 
#' Given a vector of survey responses, an insert, and a number of items,
#' this function simulates a centered careless respondent.
#' 
#' @keywords confusion matrix
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param x the length of the span of careless responses
#' @param floor the lowest possible value for a response
#' @param ceilingthe highest possible value for a response
#' @export
#' 

genCentered <- function(x, floor=1, ceiling=7) {
  mu <- 3.5
  sigma <- 1.25
  r <- round(stats::rnorm(x, mu, sigma))
  r[r>ceiling] <- ceiling
  r[r<floor] <- floor
  return(r)
}