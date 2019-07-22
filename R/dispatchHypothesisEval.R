#' An interface for testing classification methods
#' 
#' Takes a dataframe and method label as parameters, then evaluates the 
#' performance of the specified classification method, returning a
#' confusion_matrix object that contains the common classifier metrics.
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @param method a string specifying the classification method to evaluate
#' @export

dispatchHypothesisEval <- function(x, hypothesis, ...) {
  
  hypothesis1 <- function(x, ...) {
    evaluateHypothesis1(x, ...)
  }
  hypothesis2 <- function(x, ...) {
    evaluateHypothesis2(x, ...)
  }
  
  switch(hypothesis, 
         "h1" = hypothesis1(x, ...),
         "h2" = hypothesis2(x, ...)
  )
}