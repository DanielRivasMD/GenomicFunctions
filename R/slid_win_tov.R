
#' @title Window Slider with Table Overlaps
#'
#' @description
#' Takes in a numerical vector \emph{'f_seqs'} and outputs their positions assuming \emph{'d_bin_size'} and \emph{'d_bin_overlap'}
#'
#' @inheritParams slid_win_ov
#'
#' @return Table with overlap indexes
#'
#' @seealso \code{\link{slid_win}}
#'
#' @section Warning:
#' \emph{'slid_win_ov'} uses \emph{'slid_win'} to determine the window sliding
#'
#' @examples
#' slid_win_tov(1:100, 25, 5)
#' @export

slid_win_tov <- function(

	f_seq,
	d_bin_size = 500,
	d_bin_overlap = 10
) {

	f_hit_ls <- list()
	#
	for(f_which_over in 1:d_bin_overlap){
		#
		f_step_size <- d_bin_size * ((f_which_over-1) / d_bin_overlap)
		f_seqs <- f_seq + f_step_size
		f_hit_table <- table(slid_win(f_seqs, d_bin_size) - f_step_size)
		f_hit_ls[[f_which_over]] <- f_hit_table
	}

	return(f_hit_ls)
}
