#' A function for finding the index of the mode of a dataset
#' 
#' assuming the group by variable is in the first column, this function
#' finds the index of the modal value of a data frame.
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x an nx2 data frame
#' @export
indexOfMode <- function(x) {
  df <- x %>% group_by(.[,1]) %>% summarise(n = n()) %>% mutate(max=max(n))
  df[which(is.equal(df$n, df$max)),1]
}
