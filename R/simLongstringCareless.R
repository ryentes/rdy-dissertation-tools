#' Simulates a longstring careless respondent
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe containing the simulated response vector
#' @param floor the lowest possible value for each response
#' @param ceiling the highest possible value for each response
#' @export
simLongstringCareless <- function(x, ...) {
  repvalue <- sample(floor:ceiling,1)
  x[insert:nitems] <- repvalue
  return(x)
}