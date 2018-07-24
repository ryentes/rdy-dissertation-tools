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

dispatchClassifierEval <- function(x, method, ...) {
  longstring <- function(x, ...) {
    longstringClassifier(x, ...)
  }
  evenodd <- function(x, ...) {
    evenoddClassifier(x, ...)
  }
  mahad <- function(x, ...) {
    mahadClassifier(x, ...)
  }
  
  switch(method, 
         "longstring" = longstring(x, ...),
         "evenodd" = evenodd(x, ...),
         "mahad" = mahad(x, ...)
  )
}