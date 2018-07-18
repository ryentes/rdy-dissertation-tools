simLongstringCareless <- function(x, floor, ceiling) {
  repvalue <- sample(floor:ceiling,1)
  x[insert:nitems] <- repvalue
  return(x)
}