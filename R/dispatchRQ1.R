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

dispatchRQ1 <- function(x, what, ...) {
  df <- x[,1:100]
  truth <- x[,101]
  crModel <- x[,102]
  
  longstring <- function(df, truth, crModel, ...) {
    args <- list(...)
    ls <- careless::longstring(df)
    sdInquisition(ls, truth, crModel, ...)
  }
  
  evenodd <- function(df, truth, crModel, ...) {
    args <- list(...)
    eo <- careless::evenodd(df, args$factors)
    sdInquisition(eo, truth, crModel,  ...)
  }
  
  mahad <- function(df, truth, crModel, ...) {
    args <- list(...)
    mahD <- careless::mahad(df, flag=FALSE, plot=FALSE)
    sdInquisition(mahD, truth, crModel, ...)
  }
  
  switch(what, 
         "longstring" = longstring(df, truth, crModel, what, ...),
         "evenodd" = evenodd(df, truth, crModel, what, ...),
         "mahad" = mahad(df, truth, crModel, what,  ...)
  )
}