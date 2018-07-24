#' Choose a random value from a Gaussian distribution
#' 
#' the size of careless populations in my simulations have a fairly small mean
#' with a reasonable standard deviation. In this and other scenarios it's
#' possible to select random values less than 0%, which doesn't have meaning.
#' If we were to round to zero then that would alter its shape. Thus we just
#' resample until we get a valid value
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param mu the mean of the distribution
#' @param sigma the standard deviation of the distribution
#' @param seed a seed value to allow for replication
#' @export
 
getRandomGaussian <- function(mu, sigma, max) {
  x <- 0
  while(x < 1 | x > max) {
    x <- round(rnorm(1, mean=mu, sd=sigma))
  }
  return(x)
}