
#' @title Window Slider with Table Overlaps
#'
#' @description
#' Takes in a numerical vector \emph{'fSeqs'} and outputs their positions assuming \emph{'dBinSize'} and \emph{'dBinOverlap'}
#'
#' @inheritParams slidWinOv
#'
#' @return Table with overlap indexes
#'
#' @seealso \code{\link{slidWin}}
#'
#' @section Warning:
#' \emph{'slidWinOv'} uses \emph{'slidWin'} to determine the window sliding
#'
#' @examples
#' slidWinTov(1:100, 25, 5)
#' @export

slidWinTov <- function(

  fSeq,
  dBinSize = 500,
  dBinOverlap = 10
) {

  fHitLs <- list()
  #
  for(fWhichOver in 1:dBinOverlap){
  #
  fStepSize <- dBinSize * ((fWhichOver-1) / dBinOverlap)
  fSeqs <- fSeq + fStepSize
  fHitTable <- table(slidWin(fSeqs, dBinSize) - fStepSize)
  fHitLs[[fWhichOver]] <- fHitTable
  }

  return(fHitLs)
}
