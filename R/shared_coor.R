
#' @title shared_coor
#'
#' @description
#' \emph{shared_coor} takes two data.frames with start (first column) and end (second column) coordinates, and finds overlaps.
#' \emph{d_genomic} option ( default = \emph{FALSE} ) if non-genomic coordinates, otherwise chromosome information must be entered in column 3.
#' requires \strong{GenomicRanges} (built-in).
#'
#' @param f_query data.frame interpreted as query with specified format
#' @param f_subj data.frame interpreted as subject with specified format
#' @param query names to assign to query on output
#' @param subj names to assign to subject on output
#' @param d_genomic boolean as to interpret third column as chromosome
#'
#' @return outputs a list of query and subject locations.
#'
#' @seealso \code{\link{unique_coor}}
#' @seealso \code{\link{GenomicRanges::GRanges}}
#' @seealso \code{\link{IRanges::IRanges}}
#' @seealso \code{\link{IRanges::findOverlaps}}
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom IRanges findOverlaps
#'
#' @export

shared_coor <- function(

	f_query,
	f_subj,
	query,
	subj,
	d_genomic = FALSE
) {

	if ( d_genomic == FALSE ) {
		#
		f_query_ranges <- IRanges::IRanges(start = f_query[, 1], end = f_query[, 2])
		f_subj_ranges <- IRanges::IRanges(start = f_subj[, 1], end = f_subj[, 2])
	}else{
		#
		f_query_ranges <- GenomicRanges::GRanges(seqnames = f_query[, 3], IRanges::IRanges(start = f_query[, 1], end = f_query[, 2]))
		f_subj_ranges <- GenomicRanges::GRanges(seqnames = f_subj[, 3], IRanges::IRanges(start = f_subj[, 1], end = f_subj[, 2]))
	}
	f_query_subj <- as.data.frame(IRanges::findOverlaps(f_query_ranges, f_subj_ranges))
	colnames(f_query_subj) <- c(query, subj)
	shared_pos_ls <- list(f_query[f_query_subj[, query], ], f_subj[f_query_subj[, subj], ])
	names(shared_pos_ls) <- c(query, subj)

	return(shared_pos_ls)
}
