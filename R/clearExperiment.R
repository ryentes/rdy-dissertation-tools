#' A convenience function for running hypotheses
#' 
#' @keywords package loading
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param hypothesis specifies the research question or hypothesis
#' @export
clearExperiment <- function(hypothesis) {
  file.remove(glue::glue('logs/{hypothesis}/*'))
  file.remove(glue::glue('artifacts/{hypothesis}/ls/*'))
  file.remove(glue::glue('artifacts/{hypothesis}/eo/*'))
  file.remove(glue::glue('artifacts/{hypothesis}/md/*'))
}