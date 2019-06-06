
#shared_coor
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_query
#'
#' @param f_subj
#'
#' @param query
#'
#' @param subj
#'
#' @param f_genomic
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

shared_coor <- function(f_query, f_subj, query, subj, f_genomic=F){

	if(f_genomic==F){

		# require(IRanges)
		f_query_ranges <- IRanges::IRanges(start=f_query[, 1], end=f_query[, 2])
		f_subj_ranges <- IRanges::IRanges(start=f_subj[, 1], end=f_subj[, 2])
	}else{

		# require(GenomicRanges)
		f_query_ranges <- GenomicRanges::GRanges(seqnames=f_query[, 3], IRanges(start=f_query[, 1], end=f_query[, 2]))
		f_subj_ranges <- GenomicRanges::GRanges(seqnames=f_subj[, 3], IRanges(start=f_subj[, 1], end=f_subj[, 2]))
	}
	f_query_subj <- as.data.frame(IRanges::findOverlaps(f_query_ranges, f_subj_ranges))
	colnames(f_query_subj) <- c(query, subj)
	shared_pos_ls <- list(f_query[f_query_subj[, query], ], f_subj[f_query_subj[, subj], ])
	names(shared_pos_ls) <- c(query, subj)
	return(shared_pos_ls)
	# return(cbind(f_query[f_query_subj[, query], ], f_subj[f_query_subj[, subj], ]))

	# shared_coor takes two data.frames with start (first column) and end (second column) coordinates, and finds overlaps.
	# f_genomic option F if non-genomic coordinates, otherwise chromosome information must be entered in column 3.
	# requires GRanges (built-in).
	# outputs a list of query and subject locations.
}
