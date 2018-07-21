simSkewedCareless <- function(x, ...) {
  args <- list(...)
  span <- args$insert:args$nitems
  repvalue <- genSkewed(length(span))
  x[span] <- repvalue
  return(x)
}