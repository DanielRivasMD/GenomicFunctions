
#' @title Window Slider with Overlaps
#'
#' @description
#' Takes in numerical vector as \emph{'f_seq'} and outputs their positions assuming \emph{'d_bin_size'} and \emph{'d_bin_overlap'}
#'
#' @inheritParams slid_win
#'
#' @param d_bin_overlap Bin overlap. \strong{Default = 10}
#'
#' @return Numerical vector
#'
#' @seealso \code{\link{slid_win}}
#'
#' @section Warning:
#' \emph{'slid_win_ov'} is only useful for single numbers, otherwise use \emph{'slid_win_tov'}
#' \emph{'slid_win_ov'} uses \emph{'slid_win'} to determine the window sliding
#'
#' @examples
#' slid_win_ov(37, 15, 5)
#' @export

slid_win_ov <- function(

	f_seq,
	d_bin_size = 500,
	d_bin_overlap = 10
) {

	f_which_over <- 1:d_bin_overlap
	f_step_size <- d_bin_size * ((f_which_over - 1) / d_bin_overlap)

	return(slid_win(f_seq + f_step_size, d_bin_size) - f_step_size)
}
