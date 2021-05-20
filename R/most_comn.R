
#' @title Most Common
#'
#' @description
#' \emph{mostComn}
#'
#' @param fX input
#' @param naRm boolean
#'
#' @return return
#'
#' @export

mostComn <- function(

  fX,
  naRm = FALSE
) {

  if(naRm == TRUE){

  fX <- fX[!is.na(fX)]
  }

  fUx <- unique(fX)
  fUx[which.max(tabulate(match(fX, fUx)))]
}
