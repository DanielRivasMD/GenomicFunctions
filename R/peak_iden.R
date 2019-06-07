
#' @title Peak Identifier
#'
#' @description
#' Identifies peaks in a vector of number above a defined threshold \emph{'d_threshold'}
#'
#' @param f_seq Numeric vector
#' @param d_threshold Threshold for peak identification. \strong{Default = 1}
#'
#' @return data.frame with following columns: \enumerate{
#' \item peak_no
#' \item lower_lim_ix
#' \item upper_lim_ix
#' \item peak_length_ix
#' }
#'
#' @seealso \code{\link{peak_iden_seq}}
#' @examples
#' x <- sample(0:5, 100, replace = TRUE, prob = c(5, rep(1, 5)))
#' peak_iden(x)
#' @export

peak_iden <- function(

	f_seq,
	d_threshold = NULL
) {

	if ( is.null(d_threshold) ) d_threshold = 1
	f_seq <- c(0, f_seq, 0)
	f_threseq <- which(f_seq >= d_threshold)
	f_peak_length <- which(f_seq[f_threseq + 1]<d_threshold) - which(f_seq[f_threseq-1]<d_threshold) + 1
	f_upper_lim_ix <- (f_threseq[cumsum(f_peak_length)]) - 1
	f_lower_lim_ix <- f_upper_lim_ix - f_peak_length + 1
	peak_feat <- data.frame(peak_no = seq_along(f_lower_lim_ix), lower_lim_ix = f_lower_lim_ix, upper_lim_ix = f_upper_lim_ix, peak_length_ix = f_peak_length)

	return(peak_feat)
}
