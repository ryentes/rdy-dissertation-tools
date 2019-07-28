#' Evaluates Hypothesis 1 on a single dataset
#' 
#' Takes a dataframe and dots as parameters
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @param ... other parameters as necessary
#' @export

evaluateHypothesis1 <- function(x, ...) {
  args <- list(...)
  
  # Values set as outcomes of  RQ1
  # included as variables for convenience of other researchers
  LS_THRESHOLD = .4
  OUT_THRESHOLD = .5
  
  hexkey <- read.csv('E:/dis/dissertation-dev/dissertation-dev/sourcedata/hexkey.csv', header=FALSE)[1:100]
  hexkey <- which(hexkey==-1)
  
  df <- x[, 1:args$lastColumn]
  rcdf <- rdydisstools::reverseCode(df[,1:100], hexkey, max=7)
  truth <- x[, eval(args$lastColumn+1)]
  
  out <- careless::mahad(rcdf, plot=FALSE)
  outpredictions <- sdPredictor(out, OUT_THRESHOLD)

  
  #longstring
  ls <- careless::longstring(df)
  lspredictions <- sdPredictor(ls, LS_THRESHOLD)
  
  # clean using longstring
  x_lspred <- cbind(x, lspredictions)
  x_lscut <- x_lspred %>% dplyr::filter(lspredictions == 0)
  
  # new rcdf and truth with the filtered dataset
  df_lscut <- x_lscut[,1:args$lastColumn]
  df_lscut_truth <- x_lscut[, eval(args$lastColumn+1)]
  rcdf_lscut <- rdydisstools::reverseCode(df_lscut, hexkey, max=7)
  
  # outlier computation following longstring
  out2 <- careless::mahad(rcdf_lscut, plot=FALSE)
  out2predictions <- sdPredictor(out2, OUT_THRESHOLD)
  
  outCM <- confusionMatrix(outpredictions, truth)
  
  out2CM <- confusionMatrix(out2predictions, df_lscut_truth)

  return(list(outCM, out2CM))
}