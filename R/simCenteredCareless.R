#' Simulates a centered careless respondent
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe containing the simulated response vector
#' @param floor the lowest possible value for each response
#' @param ceiling the highest possible value for each response
#' @export
 
simCenteredCareless <- function(x, ...) {
  args <- list(...)
  return(glue("The primary arg is {args$wat}"))
}