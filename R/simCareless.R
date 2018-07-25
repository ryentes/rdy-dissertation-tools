#' An interface for simulating careless respondents
#' 
#' Given a matrix of item parameters that are generated from mirt this function
#' extracts the a parameter. This function is pretty specific to my dissertation
#' 
#' @keywords confusion matrix
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param x a vector of item responses
#' @param type the label, or type of careless responder
#' @param ... any additional parameters needed to simulate a respondent of that type
#' @export
#' 

simCareless <- function(x, type, ...) {
  logging::loginfo(glue::glue('Careless record generated with insert at: {insert}, and refract: {refract}'), logger='dis2.l2')
  
  longstring <- function(x, floor, ceiling, seed) {
    simLongstringCareless(x, ...)
  }
  
  skewed <- function(x, ...) {
    simSkewedCareless(x, ...)
  }
  
  centered <- function(x,  ...) {
    simCenteredCareless(x, ...)
  }
  
  logging::loginfo(glue::glue('Dispatching simulated respondent of  type: {type}'), logger='dis2.l2.')
  
  switch(type,
         "longstring" = simLongstringCareless(x, ...),
         "skewed" = simSkewedCareless(x, ...),
         "centered" = simCenteredCareless(x, ...)
  )
}