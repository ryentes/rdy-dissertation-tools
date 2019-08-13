#' A hypothesis dispatcher
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe for analysis
#' @param ... any additional parameters
#' @export

dispatchRQ2 <- function(x,  ...) {
  args <- list(...)
  
  truth <- x[,eval(args$lastColumn+1)]
  rcx <- cbind(rdydisstools::reverseCode(x[,1:100], hexkey, max=7), x[,eval(args$lastColumn+1):ncol(x)])
  rcdf <- rcx[,1:args$lastColumn]
  df <- x[,1:args$lastColumn]
  
  
  # ls simultaneous
  l <- longstringClassifier(df, truth, cut=.4)
  ls_simul_preds <- l$predictions
  ls_simul_info <- l$informedness
  
  # eo simultaneous
  e <- evenoddClassifier(rcx, truth, cut=.2)
  eo_simul_preds <- e$predictions
  eo_simul_info <- e$informedness
  
  # out simultaneous
  o <- mahadClassifier(rcdf, truth, cut= .5)
  out_simul_preds <- o$predictions
  out_simul_info <- o$informedness
  
  
  # clean using longstring
  x_lspred <- cbind(x, ls_simul_preds)
  x_lscut <- x_lspred %>% dplyr::filter(ls_simul_preds == 0)
  df_lscut <- x_lscut[,1:args$lastColumn]
  df_lscut_truth <- x_lscut[, eval(args$lastColumn+1)]
  rcdf_lscut <- rdydisstools::reverseCode(df_lscut, hexkey, max=7)
  
  
  e2 <- evenoddClassifier(rcdf_lscut, df_lscut_truth, cut=.2)
  eo_pls_preds <- e2$predictions
  eo_pls_info <- e2$informedness
  
  o2 <- mahadClassifier(rcdf_lscut, df_lscut_truth, cut=.5)
  out_pls_preds <- o2$predictions
  out_pls_info <- o2$predictions
  
  votes <- cbind(truth, ls_simul_preds, eo_simul_preds, out_simul_preds) %>% as.data.frame %>% mutate(eo_pls_preds=0, out_pls_preds=0)
           
  lsUncut <- which(ls_simul_preds == 0)
  
  votes$eo_pls_preds[lsUncut] <- eo_pls_preds
  votes$out_pls_preds[lsUncut] <- out_pls_preds
  
  
  return(votes)
}