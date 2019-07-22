#' Implements the mahad classifier evaluation
#' 
#' Takes a dataframe as an argument and evaluates the performance of mahad
#' as a classifier for that dataset.
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @export

mahadClassifier <- function(x, ...) {
  args <- list(...)
  
  df <- x[, 1:args$lastColumn]
  truth <- x[, eval(args$lastColumn+1):ncol(x)]
  md <- careless::mahad(df, plot=FALSE)
  xbar <- base::mean(md)
  SD <- stats::sd(md)
  
  predictions <- ifelse(md > xbar + (SD*args$cut), 1, 0)
  
  confusionMatrix(predictions, truth[,1])
}