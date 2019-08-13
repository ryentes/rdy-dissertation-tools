#' Implements the evenodd classifier evaluation
#' 
#' Takes a dataframe as an argument and evaluates the performance of evenodd
#' as a classifier for that dataset.
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @export

evenoddClassifier <- function(x, truth, cut) {
  
  eo <-  careless::evenodd(x, factors=rep(10,10))
  xbar <- base::mean(eo)
  SD <- stats::sd(eo)
  
  predictions <- ifelse(eo >= xbar + (SD*cut), 1, 0)
  
  confusionMatrix(predictions, truth)
}