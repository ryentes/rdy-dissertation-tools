#' A function for finding y of x
#' 
#' This functiont takes a two column data frame as input. It
#' computes the max of the second column, and then returns the first
#' corresponding value from the first column
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x an nx2 data frame
#' @export
maxLookup <- function(x) {
  max <- max(x[,2])
  isMax <- which(x[,2] == max)
  if (length(isMax) > 1) { return(x[isMax[1],1])}
  else {return(x[isMax,1])}
}