
#' @title peakTag
#'
#' @description
#' \emph{peakTag}
#'
#' @param fDfPos fDfPos
#' @param fSeqDist fSeqDist
#'
#' @return return
#'
#' @export

peakTag <- function(

  fDfPos,
  fSeqDist
) {

  outVal <- data.frame(maxIx=which(c(fDfPos[-1], 0) - fDfPos != fSeqDist))
  outVal$minIx <- c(1, outVal$maxIx[-length(outVal$maxIx)] + 1)

  return(outVal)
}
