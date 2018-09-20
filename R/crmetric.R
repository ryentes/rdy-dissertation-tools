#' Implements a crmetric class
#' 
#' This class returns a crmetric object
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param metric the metric in question
#' @param careless a binary vector indicating whether each respondent is careless
#' @param crModel a factor containing the CR model each careless respondent was drawn from
#' @export

crmetric <- function(metric, careless, crModel) {
  j <- cbind.data.frame(metric, careless, crModel)
  attr(j, 'class') <- c('crmetric', 'data.frame')
  return(j)
}