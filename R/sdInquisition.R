#' Implements the evenodd decision rule optimizer
#' 
#' Takes a dataframe .
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param metric a dataframe on which to test the classifier
#' @param truth a binary vector of classification labels
#' @param crModel a vector of labels for the CR model from which each CR was drawn
#' @export

sdInquisition <- function(metric, truth, crModel, ...) {
  args <- list(...)
  xbar <- mean(metric)
  SD <- sd(metric)
  
  
  range <- seq(from=args$from,to=args$to,by=args$by)
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
    unlist() %>% informedness(range, .)
  
  crmetric <- crmetric(metric, truth, crModel)

  return(list(results=resultMatrices, range=range, metric=crmetric, informedness=informedness))
}