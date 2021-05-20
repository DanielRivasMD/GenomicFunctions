
#' @title locusDiff
#'
#' @description
#' \emph{locusDiff}
#'
#' @param fX fX
#' @param fY fY
#' @param fData fData
#'
#' @return return
#'
#' @seealso \code{\link{peakDiff}}
#' @seealso \code{\link{hClust}}
#' @export

locusDiff <- function(

  fX,
  fY,
  fData
) {

  outVal <- sum(abs(fData[, fX] - fData[, fY]))
  return(outVal)
}
