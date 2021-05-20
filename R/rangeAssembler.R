
#' @title Range Assembler
#'
#' @description
#' Wrapper to create GenomicRanges objects in one line
#'
#' @param fData data.frame to be transform. Must be formated as: ranges start, ranges end and seqnames
#' @param bGenom boolean to set if chromosome values are set
#'
#' @return GenomicRanges object
#'
#' @seealso \code{\link{uniqueCoor}}
#' @seealso \code{\link{sharedCoor}}
#'
#' @export

rangeAssembler <- function(

  fData,
  bGenom = TRUE
) {

  fData <- as.data.frame(fData)
  if ( bGenom ) {
    fOut <- GenomicRanges::GRanges(
      seqnames = fData[, 3],
      ranges = IRanges::IRanges(
        start = fData[, 1],
        end = fData[, 2]
      )
    )
  } else {
    fOut <- IRanges::IRanges(
      start = fData[, 1],
      end = fData[, 2]
    )
  }

  return(fOut)
}
