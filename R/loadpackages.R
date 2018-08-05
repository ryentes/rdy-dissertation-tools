#' A convenience function for loading packages
#' 
#' @keywords package loading
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param x a list of string package names
#' @export
loadpackages <- function(x=c('dplyr','ggplot2', 'grid', 'gridExtra')) {
  suppressWarnings(
    sapply(x, FUN=require, quietly=TRUE, character.only=TRUE, warn.conflicts=FALSE)
  )
}