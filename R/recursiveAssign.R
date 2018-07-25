#' Recursively assigns participants to condition
#' 
#' Given a matrix of survey responses, a number of participants to select, and 
#' a vector of conditions to select them into, this function recursively assigns
#' participants at random.
#' 
#' @keywords confusion matrix
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param x a matrix of survey responses
#' @param d a vector representing the number of records to assign to each label, i think.
#' @param a vector of labels
#' @export
#' 

recursiveAssign <- function(x, d, labels) {
  # If it's the last label, just assign whatever is left
  if(length(d) == 1) {
    x[] <- labels
  } else { 
    # Draw a sample careless respondents of size (d) for the next model (label)
    selected <- sample(1:length(x),d[1])
    selected <- is.element(1:length(x),selected)
    x[selected] <- labels[1]
    # trim the size and label variables to remove the model that has just
    # been sampled.
    labels <- labels[2:length(labels)]
    d <- d[2:length(d)]
    # Call the function again to assign the remaining careless responders to 
    # their model of careless responding
    x[!selected] <- recursiveAssign(x[!selected],d,labels)
  }
  return(x)
}
