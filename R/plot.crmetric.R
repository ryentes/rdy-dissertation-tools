#' A method for plotting crmetric objects
#' 
#' Takes an informedness object and plots it using ggplot
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x an crmetric object
#' @param metric a label for the metric on the plot
#' @param i a figure number for the graph
#' @export

plot.crmetric <- function(x, thisMetric, i) {
  names(x)[1] <- thisMetric
  ggplot2::ggplot(data=x, aes_string(x=thisMetric, y="careless", group="crModel")) + 
    geom_point(aes(color=factor(crModel))) +
    ggtitle(glue::glue('Figure {i}.2: {thisMetric} by Careless and crModel')) +
    theme(legend.position="bottom", text=element_text(family="Times New Roman", size=12))
}