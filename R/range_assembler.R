
#' @title Range Assembler
#'
#' @description
#' Wrapper to create GenomicRanges objects in one line
#'
#' @param f_data data.frame to be transform. Must be formated as: ranges start, ranges end and seqnames
#' @param b_genom boolean to set if chromosome values are set
#'
#' @return GenomicRanges object
#'
#' @seealso \code{\link{unique_coor}}
#' @seealso \code{\link{shared_coor}}
#'
#' @export

range_assembler <- function(

	f_data,
	b_genom = TRUE
) {

	f_data <- as.data.frame(f_data)
	if ( b_genom ) {
		f_out <- GenomicRanges::GRanges(
			seqnames = f_data[, 3],
			ranges = IRanges::IRanges(
				start = f_data[, 1],
				end = f_data[, 2]
			)
		)
	} else {
		f_out <- IRanges::IRanges(
			start = f_data[, 1],
			end = f_data[, 2]
		)
	}

	return(f_out)
}
