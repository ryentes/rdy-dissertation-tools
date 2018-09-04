#' Implements the evenodd classifier evaluation
#' 
#' Takes a dataframe as an argument and evaluates the performance of evenodd
#' as a classifier for that dataset.
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @export

evenoddClassifier <- function(x, ...) {
  args <- list(...)
  return(paste(args$test, args$test2))
}