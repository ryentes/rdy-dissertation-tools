#' A hypothesis dispatcher
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe for analysis
#' @param ... any additional parameters
#' @export

dispatchHypothesisEval <- function(x, hypothesis, ...) {
  
  hypothesis1 <- function(x, ...) {
    evaluateHypothesis1(x, ...)
  }
  hypothesis2 <- function(x, ...) {
    evaluateHypothesis2(x, ...)
  }
  
  switch(hypothesis, 
         "h1" = hypothesis1(x, ...),
         "h2" = hypothesis2(x, ...)
  )
}