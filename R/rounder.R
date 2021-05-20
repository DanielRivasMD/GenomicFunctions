
#' @title Rounder
#'
#' @description
#' Rounds numbers \emph{'fSeq'} to ceiling given a factor \emph{'fRoundFactor'}
#'
#' @param fSeq Numerical vector
#' @param fRoundFactor Rounding factor
#'
#' @return Numerical vector
#'
#' @examples
#' rounder(27, 10)
#' rounder(341:710, 20)
#' @export

rounder <- function(

  fSeq,
  fRoundFactor
) {

  return(ceiling((fSeq) / fRoundFactor) * fRoundFactor)
}
