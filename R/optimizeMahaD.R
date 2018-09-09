#' Implements the evenodd decision rule optimizer
#' 
#' Takes a dataframe .
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @export

optimizeMahaD <- function(x, ...) {
  args <- list(...)
  
  df <- x[,1:100]
  truth <- x[,101]
  
  mahD <- careless::mahad(df, flag=FALSE, plot=FALSE)
  xbar <- mean(mahD)
  SD <- sd(mahD)
  
  range <- seq(args$from,args$to,args$by)
  
  predictions <- matrix(nrow=nrow(df), ncol=length(range))
  
  for(i in 1:length(range)) {
    cut <- xbar + (range[i]*SD)
    predictions[,i] <-  as.numeric(mahD > cut)
  }
  
  colnames(predictions) <- range
  
  resultMatrices <- vector("list", ncol(predictions))
  
  for(i in 1:ncol(predictions)) {
    resultMatrices[[i]] <- confusionMatrix(predictions[,i], truth)
  }

  resultMatrices %>% purrr::map(~ .x[['informedness']]) %>% unlist(.) %>% return()
}