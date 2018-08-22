#' A factor score simulator
#' 
#' This function simulates factor scores for n respondents such that the sample
#' has a factor structure approximating x, a matrix of factor correlations. 
#' 
#' @keywords factor scores
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param x a matrix of factor correlations
#' @param n the number of respondents to simulate
#' @export
simFactorScores <- function(x,n) {
  nItems=dim(x)[1]
  w <- t(chol(x))
  r <- matrix(rnorm(nItems*n,0,1), nrow=nItems, ncol=n)
  return(t(w %*% r))
}