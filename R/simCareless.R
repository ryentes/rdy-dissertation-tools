# Still needs work:
# 1. figure out what's going on with seeds
# 2. probs going to pass paramaters through using dots
# 3. Check that implementations are consistant with proposal doc

simCareless <- function(x,floor=1,ceiling=7, type, seed) {
  nItems <- length(x)-2
  insert <- getRandomGaussian(mu=50, sigma=10, seed=seed)
  refract <- getRandomGaussian(mu=10, sigma=5, seed=seed)
  
  longstring <- function(x, floor, ceiling) {
    simLongstringCareless(x, floor, ceiling)
  }
  
  skewed <- function(x, floor, ceiling) {
    simSkewedCareless(x, floor, ceiling)
  }
  
  centered <- function(x, floor, ceiling) {
    simCenteredCareless(x, floor, ceiling)
  }
  
  switch(type,
         "longstring" = simLongstringCareless(x,floor, ceiling),
         "skewed" = simSkewedCareless(x, floor, ceiling),
         "centered" = simCenteredCareless(x, floor, ceiling)
  )
}