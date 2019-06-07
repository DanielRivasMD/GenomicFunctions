
#' @title Window Slider
#'
#' @description
#' Takes in a vector of numbers \emph{'f_seq'} and outputs its positions assuming \emph{'d_bin_size'}
#'
#' @param f_seq Numerical vector
#' @param d_bin_size Bin size. \strong{Default = 500}
#'
#' @return Numerical vector
#'
#' @examples
#' slid_win(37, 5)
#' slid_win(1:100, 15)
#' @export

slid_win <- function(

	f_seq,
	d_bin_size = 500
) {

	return(ceiling(((f_seq) - (d_bin_size / 2)) / d_bin_size) * d_bin_size)
}
