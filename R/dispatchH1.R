#' Dispatcher for Hypothesis 1
#' 
#' Takes a dataframe as an argument and returns the result of hypothesis 2
#' for that dataframe
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @param ... any additional arguments
#' @export

dispatchH1 <- function(x, ...) {
  df <- x[,1:100]
  truth <- x[,101]
  crModel <- x[,102]
  
  hypothesis1(df, truth,  ...)
  # outlier
  ## compute
  ## informedness
  
  # ls then outlier
  ## compute
  ## informedness
  
  # compare
}