
#' @title Peak Identifier Plus
#'
#' @description
#' Identifies peaks in a vector of number above a defined threshold \emph{'dThreshold'}.
#' Additionally, calculates \emph{'min'}, \emph{'mean'}, \emph{'sum'} and coordinates in indixes (ix) and nucleotides (nt) given \emph{'dBinSize'} and \emph{'dBinOverlaps'} assuming it is dealing with overlaping windows.
#'
#' @inheritParams peakIden
#'
#' @param dBinSize Defines bin size assuming \emph{fSeq} is a vector of sliding windows. \strong{Default = 500}
#' @param dBinOverlaps Defines bin overlaps assuming \emph{fSeq} is a vector of sliding windows. \strong{Default = 10}
#'
#' @return data.frame with following columns: \enumerate{
#' \item peakNo
#' \item seqMax
#' \item seqMin
#' \item seqMean
#' \item seqSum
#' \item lowerLimIx
#' \item upperLimIx
#' \item peakLengthIx
#' \item peakLengthNt
#' }
#'
#' @seealso \code{\link{peakIden}}
#' @examples
#' x <- sample(0:5, 100, replace = TRUE, prob = c(5, rep(1, 5)))
#' peakIdenSeq(
#'	x,
#'	dThreshold = 1,
#'	dBinSize = 10,
#'	dBinOverlaps = 1
#' )
#' @export

peakIdenSeq <- function(

  fSeq,
  dThreshold = NULL,
  dBinSize = NULL,
  dBinOverlaps = NULL
) {

  if ( is.null(dThreshold) ) dThreshold = 1
  if ( is.null(dBinSize) ) dBinSize = 500
  if ( is.null(dBinOverlaps) ) dBinOverlaps = 10

  fSeqDf <- data.frame(seqMax = fSeq, peakNo = NA)
  fSeq <- c(0, fSeq, 0)
  fThreseq <- which(fSeq >= dThreshold)
  fPeakLength <- which(fSeq[fThreseq + 1] < dThreshold) - which(fSeq[fThreseq - 1] < dThreshold) + 1
  fUpperLimIx <- (fThreseq[cumsum(fPeakLength)]) - 1
  fLowerLimIx <- fUpperLimIx - fPeakLength + 1

  fTempMat <- matrix(data = NA, nrow = length(fUpperLimIx), ncol = max(fPeakLength))
  for(i in max(fPeakLength):1){
  #
  fTempMat[which(fPeakLength >= i), i] <- i - 1 + fLowerLimIx[which(fPeakLength >= i)]
  }
  #
  fTempMat <- t(fTempMat)
  fMatMatch <- match(fTempMat, which(fSeqDf$seqMax != 0))
  rm(fTempMat)

  fPeakNo <- ceiling(which(!is.na(fMatMatch)) / max(fPeakLength))
  fSeqDf[which(fSeqDf$seqMax >= dThreshold), "peakNo"] <- fPeakNo
  peakFeat <- stats::aggregate(seqMax ~ peakNo, data = fSeqDf, FUN = max)
  peakFeat$seqMin <- stats::aggregate(seqMax ~ peakNo, data = fSeqDf, FUN = min)[, "seqMax"]
  peakFeat$seqMean <- stats::aggregate(seqMax ~ peakNo, data = fSeqDf, FUN = mean)[, "seqMax"]
 	peakFeat$seqSum <- stats::aggregate(seqMax ~ peakNo, data = fSeqDf, FUN = sum)[, "seqMax"]
  peakFeat$lowerLimIx <- fLowerLimIx
  peakFeat$upperLimIx <- fUpperLimIx
  peakFeat$peakLengthIx <- fPeakLength
  peakFeat$peakLengthNt <- (fPeakLength - dBinOverlaps) * (dBinSize / dBinOverlaps) + (dBinSize / dBinOverlaps)

  return(peakFeat)
}
