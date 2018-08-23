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

simCareless <- function(x, ...) {
  args <- list(...)
  logstring <- paste0(args$logstring, 'simcareless')
  
  cr <- x[101:102]
  x <- x[1:100]
  
  longstring <- function(x, ...) {
    simLongstringCareless(x, labels,  ...)
  }
  
  skewed <- function(x,  ...) {
    simSkewedCareless(x, ...)
  }
  
  centered <- function(x,  ...) {
    simCenteredCareless(x, ...)
  }
  
  logging::loginfo(glue::glue('Dispatching simulated respondent of  type: {cr$crModel}'), logger=logstring)
  
  switch(as.character(cr$crModel),
         "longstring" = simLongstringCareless(x, ...),
         "skewed" = simSkewedCareless(x, ...),
         "centered" = simCenteredCareless(x, ...)
  )
}