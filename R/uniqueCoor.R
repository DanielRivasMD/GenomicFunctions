
#' @title Unique Coordinate
#'
#' @description
#' \emph{uniqueCoor} takes two data.frames with start (first column) and end (second column) coordinates, and finds unique positions.
#' \emph{dGenomic} option ( default = \emph{FALSE} ) if non-genomic coordinates, otherwise chromosome information must be entered in column 3.
#' requires \strong{GenomicRanges} (built-in).
#'
#' @param fQuery data.frame interpreted as query with specified format
#' @param fSubj data.frame interpreted as subject with specified format
#' @param query Names to assign to query on output
#' @param subj Names to assign to subject on output
#' @param dGenomic boolean as to interpret third column as chromosome
#'
#' @return List of query and subject locations.
#'
#' @seealso \code{\link{sharedCoor}}
#'
#' @export

uniqueCoor <- function(

  fQuery,
  fSubj,
  query,
  subj,
  dGenomic = FALSE
) {

  fQueryRanges <- rangeAssembler(fQuery, bGenom = dGenomic)
  fSubjRanges <- rangeAssembler(fSubj, bGenom = dGenomic)

  fQuerySubj <- as.data.frame(IRanges::findOverlaps(fQueryRanges, fSubjRanges))
  colnames(fQuerySubj) <- c(query, subj)
  fQueryMatch <- match(seq_along(fQueryRanges), fQuerySubj[, query])
  uniquePosQuery <- fQuery[which(is.na(fQueryMatch)), ]
  fSubjMatch <- match(seq_along(fSubjRanges), fQuerySubj[, subj])
  uniquePosSubj <- fSubj[which(is.na(fSubjMatch)), ]
  uniquePosLs <- list(uniquePosQuery, uniquePosSubj)
  names(uniquePosLs) <- c(query, subj)

  return(uniquePosLs)
}
