
#' @title peakDiff
#'
#' @description
#' \emph{peakDiff}
#'
#' @inheritParams locusDiff
#'
#' @return return
#'
#' @seealso \code{\link{locusDiff}}
#' @seealso \code{\link{hClust}}
#' @seealso \code{\link{peakIden}}
#' @seealso \code{\link{uniqueCoor}}
#' @seealso \code{\link{sharedCoor}}
#' @export

peakDiff <- function(

  fX,
  fY,
  fData
) {

  pX <- peakIden(fData[, fX])
  pY <- peakIden(fData[, fY])
  uData <- uniqueCoor(pX[, c("lowerLimIx", "upperLimIx")], pY[, c("lowerLimIx", "upperLimIx")], "pX", "pY")
  outVal <- dim(uData$pX)[1] + dim(uData$pY)[1]

  return(outVal)
}
