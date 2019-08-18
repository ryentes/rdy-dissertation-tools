#' A hypothesis dispatcher
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe for analysis
#' @param ... any additional parameters
#' @export

dispatchRQ2 <- function(x,  ...) {
  args <- list(...)
  
  #hexkey <- read.csv('~/notebooks/dissertation/sourcedata/hexkey.csv', header=FALSE)[1:100]
  #hexkey <- which(hexkey==-1)
  
  truth <- x[,eval(args$lastColumn+1)]
  rcx <- cbind(rdydisstools::reverseCode(x[,1:100], hexkey, max=7), x[,eval(args$lastColumn+1):ncol(x)])
  rcdf <- rcx[,1:args$lastColumn]
  df <- x[,1:args$lastColumn]
  
  
  # ls simultaneous
  l <- careless::longstring(df, avg=FALSE)
  ls_xbar <- base::mean(l)
  ls_SD <- stats::sd(l)
  ls_simul_preds <- ifelse(l >= ls_xbar + (ls_SD*args$LS_CUT), 1, 0)
  
  # eo simultaneous
  e <- careless::evenodd(rcdf, factors=rep(10,10))
  eo_xbar <- base::mean(e)
  eo_SD <- stats::sd(e)
  eo_simul_preds <- ifelse(e >= eo_xbar + (eo_SD*args$EO_CUT), 1, 0)
    
  
  # out simultaneous
  o <- careless::mahad(rcdf, plot=FALSE)
  out_xbar <- base::mean(o)
  out_SD <- stats::sd(o)
  out_simul_preds <- ifelse(o > out_xbar + (out_SD*args$OUT_CUT), 1, 0)

  
  
  # clean using longstring
  x_lspred <- cbind(x, ls_simul_preds)
  x_lscut <- x_lspred %>% dplyr::filter(ls_simul_preds == 0)
  df_lscut <- x_lscut[,1:args$lastColumn]
  df_lscut_truth <- x_lscut[, eval(args$lastColumn+1)]
  rcdf_lscut <- rdydisstools::reverseCode(df_lscut, hexkey, max=7)
  
  
  e2 <- careless::evenodd(rcdf_lscut, factors=rep(10,10))
  eo2_xbar <- base::mean(e2)
  eo2_SD <- stats::sd(e2)
  eo_lsf_preds <- ifelse(e2 >= eo2_xbar + (eo2_SD*args$EO_CUT), 1, 0)

  # out simultaneous
  o2 <- careless::mahad(rcdf_lscut, plot=FALSE)
  out2_xbar <- base::mean(o2)
  out2_SD <- stats::sd(o2)
  out_lsf_preds <- ifelse(o2 > out2_xbar + (out2_SD*args$OUT_CUT), 1, 0)
  
  # Not part of diss but running 'cause interested
  outsq <- base::scale(o2, center=TRUE, scale=TRUE)
  outsq_lsf_preds <- ifelse(base::sqrt(outsq^2) > 1, 1, 0)
  
  votes <- cbind(truth, ls_simul_preds, eo_simul_preds, out_simul_preds) %>% as.data.frame %>% mutate(eo_lsf_preds=0, out_lsf_preds=0, outsq_lsf_preds=0)
  scores <- cbind(ls=l, eo=e,  out=o, outsq = base::sqrt(base::scale(o, center=TRUE, scale=TRUE)^2))
  colnames(scores)[4] <- "outsq"
  scores <- as.data.frame(scores)
           
  lsUncut <- which(ls_simul_preds == 0)
  
  votes$eo_lsf_preds[lsUncut] <- eo_lsf_preds
  votes$out_lsf_preds[lsUncut] <- out_lsf_preds
  votes$outsq_lsf_preds[lsUncut] <- outsq_lsf_preds
  
  #save(votes, file=glue::glue("~/notebooks/dissertation/artifacts/rq2/votes/sim{args$i}.RData"))
  #save(scores, file=glue::glue("~/notebooks/dissertation/artifacts/rq2/scores/sim{args$i}.RData"))
  
  vs <- voteScore(votes)
  
  return(scores)
}