#' Simulates a skewed careless respondent
#' 
#' Given a vector of survey responses, an insert, and a number of items,
#' this function simulates a skewed careless respondent.
#' 
#' @keywords confusion matrix
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param ipar matrix of item parameters from mirt
#' @param names the names of the items
#' @param nchar the number of characters to extract from the names.
#' @export
#' 

simSkewedCareless <- function(x, ...) {
  args <- list(...)
  span <- args$insert:args$nitems
  repvalue <- genSkew(length(span))
  x[span] <- repvalue
  return(x)
}