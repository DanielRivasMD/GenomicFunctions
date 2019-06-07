
#' @title Unique Coordinate
#'
#' @description
#' \emph{unique_coor} takes two data.frames with start (first column) and end (second column) coordinates, and finds unique positions.
#' \emph{d_genomic} option ( default = \emph{FALSE} ) if non-genomic coordinates, otherwise chromosome information must be entered in column 3.
#' requires \strong{GenomicRanges} (built-in).
#'
#' @param f_query data.frame interpreted as query with specified format
#' @param f_subj data.frame interpreted as subject with specified format
#' @param query Names to assign to query on output
#' @param subj Names to assign to subject on output
#' @param d_genomic boolean as to interpret third column as chromosome
#'
#' @return List of query and subject locations.
#'
#' @seealso \code{\link{shared_coor}}
#'
#' @export

unique_coor <- function(

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
	f_query_match <- match(seq_along(f_query_ranges), f_query_subj[, query])
	unique_pos_query <- f_query[which(is.na(f_query_match)), ]
	f_subj_match <- match(seq_along(f_subj_ranges), f_query_subj[, subj])
	unique_pos_subj <- f_subj[which(is.na(f_subj_match)), ]
	unique_pos_ls <- list(unique_pos_query, unique_pos_subj)
	names(unique_pos_ls) <- c(query, subj)

	return(unique_pos_ls)
}
