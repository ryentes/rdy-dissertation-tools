#' A function to calculate the CI of informedness
#' 
#' This function takes a point estimate of informedness, a simulation size,
#' and a level of confidence as parameters.
#' 
#' @keywords confidence interval
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param x a vector of values to provide predictions for
#' @param cut a scalar value representing the number of standard deviations to use as a cut score.
#' @export
sdPredictor <- function(x, cut) {
  xbar <- base::mean(x)
  SD <- stats::sd(x)
  ifelse(x >= xbar + (SD*cut), 1, 0)
}