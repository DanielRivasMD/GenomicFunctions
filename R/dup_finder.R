
#' @title dup_finder
#'
#' @description
#' \emph{dup_finder}
#'
#' @param f_vec vector
#'
#' @return return
#'
#' @export

dup_finder <- function(

	f_vec
) {

	return(duplicated(f_vec) | duplicated(f_vec, fromLast = TRUE))
}
