#' A data model object
#' 
#' This function defines a class of data model. It takes a matrix of factor
#' correlations and a matrix of item parameters as inputs, then computes the
#' model slopes then returns a dataModel object which can be used as an input
#' for data simulation.
#' 
#' @keywords data model
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param factorCorllations a matrix of factor correlations
#' @param itemParameters a matrix of IRT item parameters
#' @param nchar the number of characters to retain from the factor labels
#' @export
dataModel <- function(factorCorrelations, itemParameters, nchar) {
  a <- modelSlopes(ipar, colnames(factorCorrelations), nchar)
  
  this.dataModel <- structure(
    list(
      factorCorrelations = factorCorrelations,
      itemParameters = itemParameters,
      a = a
    ), class = "dataModel"
  )
}