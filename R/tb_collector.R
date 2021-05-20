
#' @title tbCollector
#'
#' @description
#' \emph{tbCollector} collects vector data into a table of standard format defined by expected values
#'
#' @param fData fData
#' @param fIx fIx
#' @param fExpValRange expected table range
#' @param fVal fVal
#'
#' @return return
#'
#' @seealso \code{\link{concatLs}}
#' @export

tbCollector <- function(

  fData,
  fIx,
  fExpValRange,
  fVal = NULL
) {

  # empty table for fOut
  fOut <- table(fExpValRange) - 1

  # control switch
  fRangeControl <- 0

  for(fI in fIx){
  #
  if(!is.null(fVal)){
  #
  if("data.frame" %in% class(fData[[fI]])){
  #
  fITmpTb <- table(fData[[fI]][, fVal])
  }else if(fVal == 'vector'){
  #
  fITmpTb <- table(fData[[fI]])
  }
  }else{
  #
  fITmpTb <- fData[[fI]]
  }
  #
  if(length(fITmpTb) > length(fOut)){
  #
  fRangeControl <- fRangeControl + 1
  }
  fTbMatch <- match(names(fITmpTb), names(fOut))
  fOut[fTbMatch[!is.na(fTbMatch)]] <- fOut[fTbMatch[!is.na(fTbMatch)]] + fITmpTb[!is.na(fTbMatch)]
  }

  if(fRangeControl > 0){
  #
  cat(paste0('Values out of range ', fRangeControl, ' - Never tell me the odds'))
  }

  return(fOut)
}
