#' Evaluates Hypothesis 2 on a single dataset
#' 
#' Takes a dataframe and dots as parameters
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe on which to test the classifier
#' @param ... other parameters as necessary
#' @export

evaluateHypothesis2 <- function(x, ...) {
  args <- list(...)
  
  # Values set as outcomes of  RQ1
  # included as variables for convenience of other researchers
  LS_THRESHOLD = .4
  EO_THRESHOLD = .2
  
  hexkey <- read.csv('~/notebooks/dissertation/sourcedata/hexkey.csv', header=FALSE)[1:100]
  hexkey <- which(hexkey==-1)
  
  df <- x[, 1:args$lastColumn]
  rcdf <- rdydisstools::reverseCode(df[,1:100], hexkey, max=7)
  truth <- x[, eval(args$lastColumn+1)]
  
  eo <- 0 - careless::evenodd(rcdf, factors=rep(10, 10), diag=FALSE)
  eopredictions <- sdPredictor(eo, EO_THRESHOLD)

  
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
  
  # even-odd computation following longstring
  eo2 <- 0 - careless::evenodd(rcdf_lscut, factors=rep(10,10), diag=FALSE)
  eo2predictions <- sdPredictor(eo2, EO_THRESHOLD)
  
  eoCM <- confusionMatrix(eopredictions, truth)
  eo2CM <- confusionMatrix(eo2predictions, df_lscut_truth)
  
  hc <- informCI(eoCM$informedness, n=length(eo))
  ht <- informCI(eo2CM$informedness, n=length(eo2))
  
  hasOverlap  <- ht %overlaps% hc
  moreInf <- eo2CM$informedness > eoCM$informedness
  
  hypTest <- ifelse(moreInf & !hasOverlap, 1, 0)

  overlap <- DescTools::Overlap(hc, ht)

  return(c(
            hc_b=eoCM$informedness,
            hc_lower=hc[1], 
            hc_upper=hc[2],
            ht_b=eo2CM$informedness, 
            ht_lower=ht[1], 
            ht_upper=ht[2], 
            hypTest=hypTest, 
            moreInf=as.numeric(moreInf),
            hasOverlap=as.numeric(hasOverlap), 
            overlap=overlap))
}