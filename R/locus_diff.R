
#locus_diff
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_x
#'
#' @param f_y
#'
#' @param f_data
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

locus_diff <- function(f_x, f_y, f_data){

	out_val <- sum(abs(f_data[, f_x] - f_data[, f_y]))
	return(out_val)
}
