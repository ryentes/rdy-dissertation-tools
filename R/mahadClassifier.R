#' Implements the mahad classifier evaluation
#' 
#' Takes a dataframe as an argument and evaluates the performance of mahad
#' as a classifier for that dataset.
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @export

mahadClassifier <- function(x, cut=.5) {
  args <- list(...)
  
  md <- careless::mahad(x, plot=FALSE)
  xbar <- base::mean(md)
  SD <- stats::sd(md)
  
  predictions <- ifelse(md > xbar + (SD*cut), 1, 0)
  
  return(predictions)
}