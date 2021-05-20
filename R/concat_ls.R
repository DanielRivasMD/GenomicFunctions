
#' @title Concatenate List
#'
#' @description
#' \emph{concatLs} inputs a list of elements and concatenates them
#'
#' @param fData List of elements
#'
#' @return Concataned element of the same class as input
#'
#' @export

concatLs <- function(

  fData
) {

  if ( "data.frame" %in% class(fData[[1]]) | "matrix" %in% class(fData[[1]]) ) {
    for ( fLooping in 1:length(fData) ) {
      if ( fLooping == 1 ) {
        fArray <- fData[[fLooping]]
      } else {
      if ( dim(fData[[fLooping]])[1] > 0 ) {
          fArray[(1:dim(fData[[fLooping]])[1])+dim(fArray)[1], ] <- fData[[fLooping]]
        }
      }
    }
  } else {
    for ( fLooping in 1:length(fData) ) {
      if ( fLooping == 1 ) {
        if ( class(fData[[fLooping]]) == "table" ) {
          fNames <- names(fData[[fLooping]])
        }
        fArray <- fData[[fLooping]]
      } else {
        fArray[seqAlong(fData[[fLooping]])+length(fArray)] <- fData[[fLooping]]
        if ( class(fData[[fLooping]]) == "table" ) {
          fNames[seqAlong(fData[[fLooping]])+length(fNames)] <- names(fData[[fLooping]])
        }
      }
    }
    if ( class(fData[[fLooping]]) == "table" ) {
      names(fArray) <- fNames
    }
  }

  return(fArray)
}
