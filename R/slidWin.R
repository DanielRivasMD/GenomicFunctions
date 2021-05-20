
#' @title Window Slider
#'
#' @description
#' Takes in a vector of numbers \emph{'fSeq'} and outputs its positions assuming \emph{'dBinSize'}
#'
#' @param fSeq Numerical vector
#' @param dBinSize Bin size. \strong{Default = 500}
#'
#' @return Numerical vector
#'
#' @examples
#' slidWin(37, 5)
#' slidWin(1:100, 15)
#' @export

slidWin <- function(

  fSeq,
  dBinSize = 500
) {

  return(ceiling(((fSeq) - (dBinSize / 2)) / dBinSize) * dBinSize)
}
