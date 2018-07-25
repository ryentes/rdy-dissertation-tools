#' Takes a vector or scalar and reverse codes it
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param var the value or vector of values to be recoded
#' @param max the highest value that var could theoretically have (e.g. 7 on a seven point likert scale)
#' @export

reverse <- function(var, max) {
  abs(var-(max+1))
}