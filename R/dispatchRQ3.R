#' A dispatcher for Research Question 3
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe for analysis
#' @param ... any additional parameters
#' @export
dispatchRQ3 <- function(x, ...) {
  args = list(...)
  
  # parse dataframe
  df = x %>% select(ls,eo,out) %>% scale()
  truth = x$truth
  
  lpa_result = lpaClassifier(df)
  
  if(!all(is.na(lpa_result$predictions))) {
    # Get prediction results
    
    lpa_preds = lpa_result$predictions
    
    cm = confusionMatrix(lpa_preds, truth)
    
    # Compute standard evaluation vars
    totalCareless = sum(truth)
    totalCareful = length(truth) - totalCareless
    lpa_cleaned = cbind(truth, lpa_preds) %>% as.data.frame %>% filter(lpa_preds == 0)
    lpa_rCareless = sum(lpa_cleaned$truth)
    lpa_rCareful = nrow(lpa_cleaned) - lpa_rCareless
    
    
    # Metrics for reporting
    lpa_metrics = c(
      lpa_info = cm$informedness,
      lpa_sensitivity = cm$sensitivity,
      lpa_specificity = cm$specificity,
      lpa_rCareless_prop = lpa_rCareless / (lpa_rCareless + lpa_rCareful),
      lpa_r_prop = (lpa_rCareless + lpa_rCareful) / (totalCareless + totalCareful),
      lpa_rCareful_prop = lpa_rCareful / totalCareful
    )
    
    save(list(lpa_result, lpa_metrics), file=glue::glue("~/notebooks/dissertation/artifacts/rq3/lparesults/sim{args$i}.RData"))
  }
  else {
    
    lpa_metrics = c(
      lpa_info = -999,
      lpa_sensitivity = -999,
      lpa_specificity = -999,
      lpa_rCareless_prop = -999,
      lpa_r_prop = -999,
      lpa_rCareful_prop = -999
    )
  }
  
  # Save stuff
  
  return(lpa_metrics)
}