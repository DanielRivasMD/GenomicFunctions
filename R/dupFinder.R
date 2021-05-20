
#' @title dupFinder
#'
#' @description
#' \emph{dupFinder}
#'
#' @param fVec vector
#'
#' @return return
#'
#' @export

dupFinder <- function(

  fVec
) {

  return(duplicated(fVec) | duplicated(fVec, fromLast = TRUE))
}
