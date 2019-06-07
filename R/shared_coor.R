
#' @title Shared Coordinate
#'
#' @description
#' \emph{shared_coor} takes two data.frames with start (first column) and end (second column) coordinates, and finds overlaps.
#' \emph{d_genomic} option ( default = \emph{FALSE} ) if non-genomic coordinates, otherwise chromosome information must be entered in column 3.
#' requires \strong{GenomicRanges} (built-in).
#'
#' @inheritParams unique_coor
#'
#' @return List of query and subject locations.
#'
#' @seealso \code{\link{unique_coor}}
#'
#' @export

shared_coor <- function(

	f_query,
	f_subj,
	query,
	subj,
	d_genomic = FALSE
) {

	f_query_ranges <- range_assembler(f_query, d_genom = d_genomic)
	f_subj_ranges <- range_assembler(f_subj, d_genom = d_genomic)

	f_query_subj <- as.data.frame(IRanges::findOverlaps(f_query_ranges, f_subj_ranges))
	colnames(f_query_subj) <- c(query, subj)
	shared_pos_ls <- list(f_query[f_query_subj[, query], ], f_subj[f_query_subj[, subj], ])
	names(shared_pos_ls) <- c(query, subj)

	return(shared_pos_ls)
}
