getRandomGaussian <- function(mu, sigma, seed) {
  set.seed(seed=seed)
  x <- 0
  while(x < 1) {
    x <- round(rnorm(1, mean=mu, sd=sigma))
  }
  return(x)
}