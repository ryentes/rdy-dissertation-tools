#' A convenience function for running hypotheses
#' 
#' @keywords package loading
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param hypothesis specifies the research question or hypothesis
#' @export
clearExperiment <- function(hypothesis) {
  rmfiles(glue::glue('logs/{hypothesis}/'))
  rmfiles(glue::glue('artifacts/{hypothesis}/ls/'))
  rmfiles(glue::glue('artifacts/{hypothesis}/eo/'))
  rmfiles(glue::glue('artifacts/{hypothesis}/md/'))
}