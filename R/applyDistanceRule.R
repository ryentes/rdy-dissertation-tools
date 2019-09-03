#' A function for finding the closest vector to a centroid
#' 
#' Compares a matrix of of centroid vectors to a rule based vector
#' and returns the label (i.e. index) of the closest class
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param v a rule-based vector for comparison.
#' @param m a matrix-like object of centroid vectors
#' @export
applyDistanceRule <- function(v, m) {
  d = dist(rbind(v, m), method="euclidean") %>% as.matrix %>% .[2:nrow(.),1]
  names(d) = NULL
  return(which.min(d))
}