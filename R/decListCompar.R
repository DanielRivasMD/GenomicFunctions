
#' @title List Comparer
#'
#' @description
#' Compare decoded lists \emph{fVec1} & \emph{fVec2}
#'
#' @param fVec1 vector of decoded values
#' @param fVec2 vector of decoded values
#' @param dSelector selector. \strong{Default = 0}
#'
#' @return vector of overlaping positions
#'
#' @export

decListCompar <- function(

  fVec1,
  fVec2,
  dSelector = 0
) {

  fVec1 <- fVec1 * 10
  fVecSum <- fVec1 + fVec2

  if ( dSelector == 0 ) {
  #
  return( which(fVecSum == 11) )

  } else if ( dSelector == 1 ) {
  #
  return( which(fVecSum == 10) )

  } else if ( dSelector == 2 ) {
  #
  return( which(fVecSum == 1) )
  }
}
