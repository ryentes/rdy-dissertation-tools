#' A function for classifying careless responders using LPA
#' 
#' yata yata yata
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a datafreame of simulated data
#' @export
lpaClassifier <- function(x) {
  est = suppressWarnings(estimate_profiles(x, n_profiles=3, variances="varying", covariances="varying"))
  
  if(!is.null(est$model_6_class_3$dff$Class)) {
    classes = est$model_6_class_3$dff$Class
    
    means = cbind(x, classes) %>% as.data.frame %>% group_by(classes) %>% summarize(ls=mean(ls), eo=mean(eo), out=mean(out))
    
    l = labelCarelessClasses(means=means, classes=classes)
    
    predictions = l$predictions
    matchStrength = l$matchStrength
  }
  
  else {
    predictions = NULL
    means = NULL
    matchStrength = NULL
    classes = NULL
  }
  
  lparesults = list(predictions=predictions,
       means=means, 
       matchStrength=matchStrength,
       classes = classes)
  
  return(lparesults)
}