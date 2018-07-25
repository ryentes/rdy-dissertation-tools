#' Extracts slopes from item parameters
#' 
#' Given a matrix of item parameters that are generated from mirt this function
#' extracts the a parameter. This function is pretty specific to my dissertation
#' 
#' @keywords confusion matrix
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param ipar matrix of item parameters from mirt
#' @param names the names of the items
#' @param nchar the number of characters to extract from the names.
#' @export
#' 

modelSlopes <- function(ipar, names, nchar) {
  nCol <- length(names)
  nRow <- nrow(ipar)
  a <- ipar[,1]
  x <- matrix(rep(0,nRow*nCol),nrow=nRow,ncol=nCol)
  for(i in 1:nRow) {
    n = substr(rownames(ipar)[i],1,nchar)
    x[i,which(names == n)]=a[i]
  }
  return(x)
}