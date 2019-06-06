
#slid_win_tov
#' @title
#' Window Slider with Overlaps
#'
#' @description
#' Takes in a vector of numbers 'f_seqs' and outputs their positions assuming 'f_bin_size' and 'f_bin_overlap'
#'
#' @section Warning:
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
#' Returns a table with overlap indexes
#' @export
#' @source
#'
#' @examples
#' slid_win_tov(1:100, 25, 5)

slid_win_tov <- function(f_seqs, f_bin_size=bin_size, f_bin_overlap=bin_overlaps){

	f_hit_ls <- list(NULL)

	for(f_which_over in 1:f_bin_overlap){

		f_step_size <- f_bin_size*((f_which_over-1)/f_bin_overlap)
		f_seq <- f_seqs+f_step_size
		f_hit_table <- table(slid_win(f_seq, f_bin_size)-f_step_size)
		f_hit_ls[[f_which_over]] <- f_hit_table
	}

	return(f_hit_ls)
}
