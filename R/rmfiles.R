#' A convenience function for removing files in a directory
#' 
#' @keywords package loading
#' @author Richard D. Yentes \email{ryentes@ncsu.edu}
#' @param path the path to the files to be deleted
#' @export
rmfiles <- function(path) {
  delfiles <- dir(path=path)
  file.remove(file.path(path,logdel))
}