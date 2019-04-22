#' rowise vectorisation of all.equal
#' 
#' This function returns a logical vector of all.equal evaluations
#' for each row of x and y
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a vector of values to compare to y
#' @param y a vector of values to compare to x
#' @export

is.equal <- Vectorize(FUN=function(x, y, ...) { isTRUE(all.equal(x, y)) }, vectorize.args = c("x", "y"))