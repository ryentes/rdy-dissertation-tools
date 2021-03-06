#' Implements the evenodd decision rule optimizer
#' 
#' Takes a dataframe .
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param metric a dataframe on which to test the classifier
#' @export

sdRocer <- function(metric, truth, crModel, from, to, by, ...) {
  args <- list(...)
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
  
  informedness <- resultMatrices %>% purrr::map(~ .x[['informedness']]) %>%
    cbind(informedness=.,range=range) %>% informedness(., args$i)
  
  crmetric <- cbind.data.frame(metric, truth, crModel) %>% 
    crmetric(., label=args$what, i=args$i)

  return(list(results=resultMatrices, range=range, metric=crmetric, informedness=informedness))
}