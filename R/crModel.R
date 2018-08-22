#' A careless responding model object
#' 
#' This function defines a model of careless responder. Objects using this class
#' are initialized with the following parameters
#' 
#' @keywords data model
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param label the name applied to this type of careless responding
#' @param mu the hypothetical mean occurance in a sample
#' @param sigma the hypothetical standard deviation of occurance in a sample
#' @export
crModel <- function(label, mu, sigma) {
  this.crModel <- structure(
    list(
      label = label,
      mu = mu,
      sigma = sigma
    ), class = "crModel"
  )
}