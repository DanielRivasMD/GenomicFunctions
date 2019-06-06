
#Peak identifier
#' @title
#' Peak Identifier
#'
#' @description
#' Identifies peaks in a vector of number above a defined threshold 'f_threshold'
#'
#' @section Warning:
#'
#'
#' @param f_seq
#' Vector of numbers
#' @param f_threshold
#' Threshold for peak identification. Default = 1
#' @return
#' Returns a data.frame with following columns: "peak_no", "lower_lim_ix", "upper_lim_ix", "peak_length_ix"
#' @export
#' @source
#'
#' @examples
#' x <- sample(0:5, 100, replace=T, prob = c(5, rep(1, 5)))
#' peak_iden_seq(x)

peak_iden <- function(f_seq, f_threshold=NULL){

	if(is.null(f_threshold)){f_threshold=1}
	f_seq <- c(0, f_seq, 0)
	f_threseq <- which(f_seq>=f_threshold)
	f_peak_length <- which(f_seq[f_threseq+1]<f_threshold)-which(f_seq[f_threseq-1]<f_threshold)+1
	f_upper_lim_ix <- (f_threseq[cumsum(f_peak_length)])-1
	f_lower_lim_ix <- f_upper_lim_ix-f_peak_length+1
	peak_feat <- data.frame(peak_no=seq_along(f_lower_lim_ix), lower_lim_ix=f_lower_lim_ix, upper_lim_ix=f_upper_lim_ix, peak_length_ix=f_peak_length)
	return(peak_feat)
}
