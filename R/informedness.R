#' Implements an informedness class
#' 
#' This class returns an informedness object
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param range the range over which informedness was computed
#' @param informedness the valus for informedness
#' @export

informedness <- function(range, informedness) {
  j <- cbind.data.frame(range, informedness)
  attr(j, 'class') <- c('informedness', 'data.frame')
  return(j)
}