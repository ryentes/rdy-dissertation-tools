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
  tpr <- tp / sum(observed)
  tnr <- tn / (1-sum(observed))
  fpr <- fp / sum(observed)
  fnr <- fn / (1-sum(observed))
  tna <- tn / (1-sum(predictions))
  tpa <- tp / sum(predictions)
  fna <- fn / (1-sum(predictions))
  fpa <- fp / sum(predictions)
  informedness <- tpr-fpr
  markedness <- tpa-fna
  roc <- AUC::roc(predictions, observed)
  auc <- AUC::auc(roc)
  rocplot <- graphics::plot(roc)
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
      tna = tna,
      tpa = tpa,
      fna = fna,
      fpa = fpa,
      informedness = informedness,
      markedness = markedness,
      roc = roc,
      auc = auc,
      rocplot = rocplot
    ), class="confusionMatrix"
  )
  
  ## Set the name for the class
  return(this_matrix)
}