
#' @title Window Slider with Overlaps
#'
#' @description
#' Takes in numerical vector as \emph{'fSeq'} and outputs their positions assuming \emph{'dBinSize'} and \emph{'dBinOverlap'}
#'
#' @inheritParams slidWin
#'
#' @param dBinOverlap Bin overlap. \strong{Default = 10}
#'
#' @return Numerical vector
#'
#' @seealso \code{\link{slidWin}}
#'
#' @section Warning:
#' \emph{'slidWinOv'} is only useful for single numbers, otherwise use \emph{'slidWinTov'}
#' \emph{'slidWinOv'} uses \emph{'slidWin'} to determine the window sliding
#'
#' @examples
#' slidWinOv(37, 15, 5)
#' @export

slidWinOv <- function(

  fSeq,
  dBinSize = 500,
  dBinOverlap = 10
) {

  fWhichOver <- 1:dBinOverlap
  fStepSize <- dBinSize * ((fWhichOver - 1) / dBinOverlap)

  return(slidWin(fSeq + fStepSize, dBinSize) - fStepSize)
}
