confusion_matrix <- function(predictions, observed)
{
  ## Compute the necessary indices of accuracy
  tp <- sum(predictions == 1 & observed == 1) 
  fp <- sum(predictions == 1 & observed == 0)
  tn <- sum(predictions == 0 & observed == 0)
  fn <- sum(predictions == 0 & observed == 1) 
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
      fn = fn
    ), class="confusion_matrix"
  )
  
  ## Set the name for the class
  return(this_matrix)
}