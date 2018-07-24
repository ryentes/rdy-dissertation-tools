# Still needs work:
# 1. separate all work into functions and call them in the respective methods (necessary for dependency inversion)
# 2. definitely passing the parameters as dots
# 3. Check that implementations are consistant with proposal doc

simCareless <- function(x, type, ...) {
  nItems <- length(x)
  insert <- getRandomGaussian(mu=50, sigma=10, seed=seed)
  refract <- getRandomGaussian(mu=10, sigma=5, seed=seed)
  
  loginfo(glue('Careless record generated with insert at: {insert}, and refract: {refract}'), logger='dis2.l2')
  
  longstring <- function(x, floor, ceiling, seed) {
    simLongstringCareless(x, ...)
  }
  
  skewed <- function(x, ...) {
    simSkewedCareless(x, ...)
  }
  
  centered <- function(x,  ...) {
    simCenteredCareless(x, ...)
  }
  
  loginfo(paste('Dispatching simulated respondent of  type: ', type), logger='dis2.l2.')
  
  switch(type,
         "longstring" = simLongstringCareless(x, ...),
         "skewed" = simSkewedCareless(x, ...),
         "centered" = simCenteredCareless(x, ...)
  )
}