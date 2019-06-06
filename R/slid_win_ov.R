
#slid_win_ov
#' @title
#' Window Slider with Overlaps
#'
#' @description
#' Takes in a vector of numbers 'f_seq' and outputs their positions assuming 'f_bin_size' and 'f_bin_overlap'
#'
#' @section Warning:
#' 'slid_win_ov' is only useful for single numbers, otherwise use 'slid_win_tov'
#' 'slid_win_ov' uses 'slid_win' to determine the window sliding
#'
#' @section See also:
#' 'slid_win'
#'
#' @param f_seq
#' Vector of numbers
#' @param f_bin_size
#' Bin size
#' @param f_bin_overlap
#' Bin overlap
#' @return
#' Returns a vector of numbers
#' @export
#' @source
#'
#' @examples
#' slid_win_ov(37, 15, 5)

slid_win_ov <- function(f_seq, f_bin_size=bin_size, f_bin_overlap=bin_overlaps){

	f_which_over <- 1:f_bin_overlap
	f_step_size <- f_bin_size*((f_which_over-1)/f_bin_overlap)
	return(slid_win(f_seq+f_step_size, f_bin_size)-f_step_size)
}
