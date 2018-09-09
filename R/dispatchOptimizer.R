#' An interface for optimizing classification methods
#' 
#' Takes a dataframe and a label for what you're optimizing as parameters, 
#' then it optimizes the specified classifier.
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @param what a string specifying the index to optimize
#' @param ... any additional arguments
#' @export

dispatchRocer <- function(x, what, ...) {
  df <- x[,1:100]
  truth <- x[,101]
  
  longstring <- function(df, truth, ...) {
    args <- list(...)
    ls <- careless::longstring(df)
    sdRocer(ls, truth, from=args$from, to=args$to, by=args$by)
  }
  
  evenodd <- function(df, truth, ...) {
    args <- list(...)
  }
  mahad <- function(df, truth, ...) {
    args <- list(...)
    mahD <- careless::mahad(df, flag=FALSE, plot=FALSE)
    
    sdRocer(mahD, truth, from=args$from, to=args$to, by=args$by)
  }
  
  switch(what, 
         "longstring" = longstring(df, truth, ...),
         "evenodd" = evenodd(df, truth, ...),
         "mahad" = mahad(df, truth,  ...)
  )
}