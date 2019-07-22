#' Implements the longstring classifier evaluation
#' 
#' Takes a dataframe as an argument and evaluates the performance of longstring
#' as a classifier for that dataset.
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @export

longstringClassifier <- function(x, ...) {
  args <- list(...)
  
  df <- x[, 1:args$lastColumn]
  truth <- x[, eval(args$lastColumn+1):ncol(x)]
  ls <- careless::longstring(df, avg=FALSE)
  xbar <- base::mean(ls)
  SD <- stats::sd(ls)
    
  predictions <- ifelse(ls >= xbar + (SD*args$cut), 1, 0)
    
  confusionMatrix(predictions, truth[,1])
}