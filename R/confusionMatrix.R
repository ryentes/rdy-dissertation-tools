#' A confusion matrix object
#' 
#' This function defines a class of confusion matrix. It takes a prediction
#' vector and an observed vector as inputs, and returns a list like object
#' containing the confusion matrix and the relevant indices of accuracy already
#' pre-computed.
#' 
#' @keywords confusion matrix
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param predictions a vector of predicted states (e.g. 0 or 1)
#' @param observed a vector of observed or actual states
#' @export

confusionMatrix <- function(predictions, observed)
{
  ## Compute the necessary indices of accuracy
  tp <- sum(predictions == 1 & observed == 1)
  fp <- sum(predictions == 1 & observed == 0) 
  tn <- sum(predictions == 0 & observed == 0) 
  fn <- sum(predictions == 0 & observed == 1)
  tpr <- tp / (tp + fp)
  tnr <- tn / (tn + fp)
  fpr <- fp / (fp + tn)
  fnr <- fn / (fn + tp)
  tna <- tn / (tn + fn)
  tpa <- tp / (tp + fp)
  fna <- fn / (1-sum(predictions)) # no idea if these are correct.
  fpa <- fp / sum(predictions)
  informedness <- (tp/(tp+fn)) + (tn/(tn+fp)) - 1
  sensitivity <- tp/(tp+fn)
  specificity <- tn/(tn+fp)
  markedness <- tpa-fna
  roc <- AUC::roc(factor(predictions), factor(observed))
  auc <- AUC::auc(roc)
  matrix <- table(predictions, observed)
  
  ## Load it up into the object
  this_matrix <- structure(
    list(
      predictions = predictions,
      observed = observed,
      matrix = matrix,
      tp = tp,
      fp = fp,
      tn = tn,
      fn = fn,
      tpr = tpr,
      tnr = tnr,
      fpr = fpr,
      fnr = fnr,
      tna = tna,
      tpa = tpa,
      informedness = informedness,
      sensitivity = sensitivity,
      specificity = specificity,
      markedness = markedness,
      auc = auc
    ), class="confusionMatrix"
  )
  
  ## Set the name for the class
  return(this_matrix)
}