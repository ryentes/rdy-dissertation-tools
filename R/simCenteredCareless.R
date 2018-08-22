#' Simulates a centered careless respondent
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe containing the simulated response vector
#' @param floor the lowest possible value for each response
#' @param ceiling the highest possible value for each response
#' @export
 
simCenteredCareless <- function(x, ...) {
  args <- list(...)
  nItems <- length(x)
  insert <- getRandomGaussian(mu=50, sigma=10, max=nItems)
  span <- insert:nItems
  replaceSpan <- genCentered(length(span), floor=args$floor, ceiling=args$ceiling)
  x[span] <- replaceSpan
  return(x)
}