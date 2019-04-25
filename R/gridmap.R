#' A function for creating an arrangement of plots. 
#' 
#' This takes a list of graph objects and arrangese them in a grid. I modified
#' a function I shamelessly took from stack overflow
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param ... a series of plot objects
#' @param plotlist optionally a list of plot objects
#' @param cols the number of columns for the grid
#' @param layout a layout for the grid object
#' @export
gridmap <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- t(matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols)))
  }
  
  if (numPlots==1) {
    return(plots[[1]])
    
  } else {
    return(grid.arrange(grobs=plots, layout_matrix=layout))
  }
}