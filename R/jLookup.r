#' A function for finding y of x
#' 
#' This functiont takes a two column data frame as input. It
#' computes the max of the second column, and the informedness corresponding
#' to i
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x an nx2 data frame
#' @param i index at which to report informedness
#' @export
jLookup <- function(x, i) {
  j = which(x[,1]==i)
  return(x[j,2])
}