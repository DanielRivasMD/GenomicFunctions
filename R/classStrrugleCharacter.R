
#' @title classStrrugleCharacter
#'
#' @description
#' \emph{classStrrugleCharacter}
#'
#' @param fDf fDf
#' @param fClass fClass
#'
#' @return return
#'
#' @seealso \code{\link{classStrrugleNumeric}}
#' @export

classStrrugleCharacter <- function(

  fDf,
  fClass
) {

  for(i in which(sapply(fDf, class) == fClass)) fDf[[i]] = as.character(fDf[[i]])
  return(fDf)
}
