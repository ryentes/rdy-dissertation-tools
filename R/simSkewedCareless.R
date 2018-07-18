simSkewedCareless <- function(x, floor, ceiling) {
  span <- insert:nitems
  repvalue <- genSkewed(length(span))
  x[span] <- repvalue
  return(x)
}