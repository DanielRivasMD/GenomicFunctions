
#' @title Hierarchical Cluster Tool
#'
#' @description
#' \emph{hClust} provides hierarchical clustering for group(s) of populations, i. e. it calculates through \emph{peakDiff} and constructs a matrix of the absolute differences among populations
#' It recives a data.frame of populations as \emph{fDataset} where a vector of column names to be compare must be specified as \emph{fPop}
#'
#' @param fDataset matrix to values
#' @param fPop population names
#'
#' @return return
#'
#' @seealso \code{\link{locusDiff}}
#' @seealso \code{\link{peakDiff}}
#' @export

hClust <- function(

  fDataset,
  fPop
) {

  fNPop <- length(fPop)
  hcluster <- matrix(data = mapply(peakDiff, fX = rep(1:fNPop, times = fNPop), fY = rep(1:fNPop, each = fNPop), MoreArgs = list(fData = fDataset[, fPop])), nrow = fNPop)
  diag(hcluster) <- NA
  colnames(hcluster) <- fPop
  rownames(hcluster) <- fPop

  return(hcluster)
}
