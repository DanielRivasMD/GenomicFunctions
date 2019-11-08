
#' @title List Comparer
#'
#' @description
#' Compare decoded lists \emph{f_vec1} & \emph{f_vec2}
#'
#' @param f_vec1 vector of decoded values
#' @param f_vec2 vector of decoded values
#' @param d_selector selector. \strong{Default = 0}
#'
#' @return vector of overlaping positions
#'
#' @export

dec_list_compar <- function(

	f_vec1,
	f_vec2,
	d_selector = 0
) {

	f_vec1 <- f_vec1 * 10
	f_vec_sum <- f_vec1 + f_vec2

	if ( d_selector == 0 ) {
		#
		return( which(f_vec_sum == 11) )

	} else if ( d_selector == 1 ) {
		#
		return( which(f_vec_sum == 10) )

	} else if ( d_selector == 2 ) {
		#
		return( which(f_vec_sum == 1) )
	}
}
