#' Simulates a longstring careless respondent
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe containing the simulated response vector
#' @param floor the lowest possible value for each response
#' @param ceiling the highest possible value for each response
#' @export

simLongstringCareless <- function(x, ...) {
  args <- list(...)
  nItems <- length(x)
  insert <- getRandomGaussian(mu=50, sigma=10)
  refract <- getRandomGaussian(mu=10, sigma=5)
  
  repvalue <- sample(args$floor:args$ceiling,1)
  x[insert:nItems] <- repvalue
  return(x)
}