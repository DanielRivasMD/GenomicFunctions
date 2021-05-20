
#' @title classStrrugleNumeric
#'
#' @description
#' \emph{classStrrugleNumeric}
#'
#' @inheritParams classStrrugleCharacter
#'
#' @return return
#'
#' @seealso \code{\link{classStrrugleCharacter}}
#' @export

classStrrugleNumeric <- function(

  fDf,
  fClass
) {

  for(i in which(sapply(fDf, class) == fClass)) fDf[[i]] = as.numeric(fDf[[i]])
  return(fDf)
}
