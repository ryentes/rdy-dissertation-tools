#' Reverse coding for an entire measure
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a dataframe containing the construct items
#' @param neg a vector of integers denoting the negatively worded items
#' @param max the highest value that var could theoretically have (e.g. 7 on a seven point likert scale)
#' @export

reverseCode <- function(x, neg, max) {
  for(i in 1:length(neg)) {
    x[,neg[i]] <- reverse(x[,neg[i]], max)
  }
  return(x)
}