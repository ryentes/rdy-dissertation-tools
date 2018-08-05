#' Assigns rownames, because rbind.fill doesn't support these.
#' 
#' @keywords package loading
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param n a character list of rownames to assign
#' @export
makeRownames <- function(n) {
  rnames <- vector(mode="character")
  for (i in 1:length(n)) {
    rnames <- c(rnames, colnames(eval(as.name(n[i]))))
  }
  rnames
}