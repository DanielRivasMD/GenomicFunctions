
#' @title peak_iden_seq
#'
#' @description
#' Identifies peaks in a vector of number above a defined threshold \emph{'f_threshold'}.
#' Additionally, calculates \emph{'min'}, \emph{'mean'}, \emph{'sum'} and coordinates in indixes (ix) and nucleotides (nt) given \emph{'f_bin_size'} and \emph{'f_bin_overlaps'} assuming it is dealing with overlaping windows.
#'
#' @param f_seq Vector of numbers
#' @param f_threshold Threshold for peak identification. Default = 1
#' @param f_bin_size Defines bin size assuming f_seq is a vector of sliding windows. Default = 'bin_size'
#' @param f_bin_overlaps Defines bin overlaps assuming f_seq is a vector of sliding windows. Default = 'bin_overlaps'
#'
#' @return Returns a data.frame with following columns: "peak_no", "seq_max", "seq_min", "seq_mean", "seq_sum", "lower_lim_ix", "upper_lim_ix", "peak_length_ix", "peak_length_nt"
#' @export
#'
#' @examples
#' x <- sample(0:5, 100, replace = T, prob = c(5, rep(1, 5)))
#' peak_iden_seq(
#'	x,
#'	f_threshold = 1,
#'	f_bin_size = 10,
#'	f_bin_overlaps = 1
#' )

peak_iden_seq <- function(

	f_seq,
	f_threshold = NULL,
	f_bin_size = NULL,
	f_bin_overlaps = NULL
) {

	if ( is.null(f_threshold) ) f_threshold = 1
	if ( is.null(f_bin_size) ) f_bin_size = bin_size
	if ( is.null(f_bin_overlaps) ) f_bin_overlaps = bin_overlaps

	f_seq_df <- data.frame(seq_max = f_seq, peak_no = NA)
	f_seq <- c(0, f_seq, 0)
	f_threseq <- which(f_seq >= f_threshold)
	f_peak_length <- which(f_seq[f_threseq + 1] < f_threshold) - which(f_seq[f_threseq-1]<f_threshold) + 1
	f_upper_lim_ix <- (f_threseq[cumsum(f_peak_length)]) - 1
	f_lower_lim_ix <- f_upper_lim_ix - f_peak_length + 1

	f_temp_mat <- matrix(data = NA, nrow = length(f_upper_lim_ix), ncol = max(f_peak_length))
	for(i in max(f_peak_length):1){
		#
		f_temp_mat[which(f_peak_length >= i), i] <- i - 1 + f_lower_lim_ix[which(f_peak_length >= i)]
	}
	#
	f_temp_mat <- t(f_temp_mat)
	f_mat_match <- match(f_temp_mat, which(f_seq_df$seq_max != 0))
	rm(f_temp_mat)

	f_peak_no <- ceiling(which(!is.na(f_mat_match)) / max(f_peak_length))
	f_seq_df[which(f_seq_df$seq_max >= f_threshold), "peak_no"] <- f_peak_no
	peak_feat <- aggregate(seq_max ~ peak_no, data = f_seq_df, FUN = max)
	peak_feat$seq_min <- aggregate(seq_max ~ peak_no, data = f_seq_df, FUN = min)[, "seq_max"]
	peak_feat$seq_mean <- aggregate(seq_max ~ peak_no, data = f_seq_df, FUN = mean)[, "seq_max"]
 	peak_feat$seq_sum <- aggregate(seq_max ~ peak_no, data = f_seq_df, FUN = sum)[, "seq_max"]
	peak_feat$lower_lim_ix <- f_lower_lim_ix
	peak_feat$upper_lim_ix <- f_upper_lim_ix
	peak_feat$peak_length_ix <- f_peak_length
	peak_feat$peak_length_nt <- (f_peak_length-f_bin_overlaps) * (f_bin_size / f_bin_overlaps) + (f_bin_size / f_bin_overlaps)

	return(peak_feat)
}
