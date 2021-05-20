
#' @title Coordinate Sequencer
#'
#' @description
#' Generates a vector given a vector of start coordinates 'fStartCoor' and a vector of end coordinates 'fEndCoor'
#'
#' @param fStartCoor Vector of start coordinates
#' @param fEndCoor Vector of end coordinates
#'
#' @return Vector sequence of concatenated integers in the same order as the coordinates
#'
#' @examples
#' x <- sample(1:100, 10)
#' y <- x + sample(1:10, 10, replace = TRUE)
#' coorSeq(x, y)
#' @export

coorSeq <- function(

  fStartCoor,
  fEndCoor
) {

  fCoorLength <- abs(fEndCoor) - abs(fStartCoor) + 1
  fDimension <- c(max(fCoorLength), length(fCoorLength))
  fTempMat <- matrix(data = NA, nrow = min(fDimension), ncol = max(fDimension))

  # long vector short interval
  if(which.min(fDimension) == 1){

  for(fPeak in min(fDimension):1){

    fWhichPeakHeight <- which(fCoorLength >= fPeak)
    fTempMat[fPeak, fWhichPeakHeight] <- fStartCoor[fWhichPeakHeight] + fPeak - 1
  }
  # long interval short vector
  }else{

    fTempMat <- t(fTempMat)

    for(fPeak in min(fDimension):1){
      fTempMat[, fPeak] <- c(fStartCoor[fPeak]:fEndCoor[fPeak], rep(NA, max(fCoorLength)-fCoorLength[fPeak]))
    }
  }

  fPeakVec <- fTempMat[which(!is.na(fTempMat))]
  return(fPeakVec)
}
