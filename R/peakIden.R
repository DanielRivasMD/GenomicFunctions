
#' @title Peak Identifier
#'
#' @description
#' Identifies peaks in a vector of number above a defined threshold \emph{'dThreshold'}
#'
#' @param fSeq Numeric vector
#' @param dThreshold Threshold for peak identification. \strong{Default = 1}
#'
#' @return data.frame with following columns: \enumerate{
#' \item peakNo
#' \item lowerLimIx
#' \item upperLimIx
#' \item peakLengthIx
#' }
#'
#' @seealso \code{\link{peakIdenSeq}}
#' @examples
#' x <- sample(0:5, 100, replace = TRUE, prob = c(5, rep(1, 5)))
#' peakIden(x)
#' @export

peakIden <- function(

  fSeq,
  dThreshold = NULL
) {

  if ( is.null(dThreshold) ) dThreshold <- 1
  fSeq <- c(0, fSeq, 0)
  fThreseq <- which(fSeq >= dThreshold)
  fPeakLength <- which(fSeq[fThreseq + 1] < dThreshold) - which(fSeq[fThreseq-1] < dThreshold) + 1
  fUpperLimIx <- (fThreseq[cumsum(fPeakLength)]) - 1
  fLowerLimIx <- fUpperLimIx - fPeakLength + 1
  peakFeat <- data.frame(peakNo = seq_along(fLowerLimIx), lowerLimIx = fLowerLimIx, upperLimIx = fUpperLimIx, peakLengthIx = fPeakLength)

  return(peakFeat)
}
