
#' @title Shared Coordinate
#'
#' @description
#' \emph{sharedCoor} takes two data.frames with start (first column) and end (second column) coordinates, and finds overlaps.
#' \emph{dGenomic} option ( default = \emph{FALSE} ) if non-genomic coordinates, otherwise chromosome information must be entered in column 3.
#' requires \strong{GenomicRanges} (built-in).
#'
#' @inheritParams uniqueCoor
#'
#' @return List of query and subject locations.
#'
#' @seealso \code{\link{uniqueCoor}}
#'
#' @export

sharedCoor <- function(

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
  sharedPosLs <- list(fQuery[fQuerySubj[, query], ], fSubj[fQuerySubj[, subj], ])
  names(sharedPosLs) <- c(query, subj)

  return(sharedPosLs)
}
