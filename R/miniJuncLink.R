
#' @title Mini Junction Linker
#'
#' @description
#' \emph{miniJuncLink}
#'
#' @param fDf data.frame
#' @param fUppCol Column interpreted as upstream
#' @param fDownCol Column interpreted as downstream
#' @param fPosCol Positions column
#' @param fThres Threshold
#'
#' @return return
#'
#' @export

miniJuncLink <- function(

  fDf,
  fUppCol,
  fDownCol,
  fPosCol,
  fThres = 1
) {

  fTmpDf <- fDf
  fTmpDf <- fTmpDf[which(fTmpDf[, fUppCol] >= 1 | fTmpDf[, fDownCol] >= 1), c(fPosCol, fUppCol, fDownCol)]
  fTmpEd <- fTmpDf[which(fTmpDf[, fUppCol] >= fThres) + 1, ]
  fVecEd <- fTmpEd[which(fTmpEd[, fUppCol] < fThres), fPosCol]
  fVecEu <- fTmpEd[which(fTmpEd[, fUppCol] < fThres) - 1, fPosCol]
  fDownPos <- match(fVecEd, fDf[, fPosCol])
  fUpPos <- match(fVecEu, fDf[, fPosCol])

  fDfE <- fDf[c(rbind(fUpPos, fDownPos)), c(fPosCol, fUppCol, fDownCol)]
  fDfE[, "dist"] <- c(fDfE[-1, fPosCol], fDfE[dim(fDfE)[1], fPosCol]) - fDfE[, fPosCol]
  fDfE[which((1:(dim(fDfE)[1]) %% 2) == 0), "dist"] <- NA

  return(fDfE)
}
