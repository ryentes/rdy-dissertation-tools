#' A method for plotting informedness objects
#' 
#' Takes an informedness object and plots it using ggplot
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x an informedness object
#' @param i a figure number for the graph
#' @export

plot.informedness <- function(x, i) {
  ggplot2::ggplot(x, aes(x=range, y=informedness)) + 
    geom_line() + geom_point() + 
    ggtitle(glue::glue('Figure {i}.1: Informedness by cut score')) +
    theme(legend.position="bottom", text=element_text(family="Times New Roman", size=12))
}