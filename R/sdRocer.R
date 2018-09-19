#' Implements the evenodd decision rule optimizer
#' 
#' Takes a dataframe .
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param metric a dataframe on which to test the classifier
#' @export

sdRocer <- function(metric, truth, from, to, by) {
  xbar <- mean(metric)
  SD <- sd(metric)
  
  range <- seq(from=from,to=to,by=by)
  rangeLength <- length(range)
  
  predictions <- matrix(nrow=nrow(df), ncol=rangeLength)
  
  for(i in 1:rangeLength) {
    cut <- xbar + (range[i]*SD)
    predictions[,i] <-  as.numeric(metric > cut)
  }
  
  colnames(predictions) <- range
  
  resultMatrices <- vector("list", ncol(predictions))
  
  for(i in 1:ncol(predictions)) {
    resultMatrices[[i]] <- confusionMatrix(predictions[,i], truth)
  }

  return(list(results=resultMatrices, metric=metric))
}