
#dup_finder
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_vec
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

dup_finder <- function(f_vec){

	return(duplicated(f_vec) | duplicated(f_vec, fromLast = TRUE))
}
