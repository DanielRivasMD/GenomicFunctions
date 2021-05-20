
#' @title Tag Decoder
#'
#' @description
#' \emph{decTag} decodes a numeric vector given a specified position \emph{fDec} by digits as \emph{dNumerator}
#'
#' @param fSeq Numerical vector
#' @param fDec Position to decode
#' @param dNumerator Digit position
#'
#' @return Numerical vector of decoded values
#'
#' @examples
#' x <- c(10.34, 435.5002, 436.7879)
#' decTag(x, 0.01)
#' @export

decTag <- function(

  fSeq,
  fDec,
  dNumerator = 100
) {

  fSeq <- (fSeq / fDec / dNumerator) + 0.0000001
  return(floor((fSeq - floor(fSeq)) * dNumerator))
}
