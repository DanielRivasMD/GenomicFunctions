
#' @title Rounder
#'
#' @description
#' Rounds numbers \emph{'f_seq'} to ceiling given a factor \emph{'f_round_factor'}
#'
#' @param f_seq Numerical vector
#' @param f_round_factor Rounding factor
#'
#' @return Numerical vector
#'
#' @examples
#' rounder(27, 10)
#' rounder(341:710, 20)
#' @export

rounder <- function(

	f_seq,
	f_round_factor
) {

	return(ceiling((f_seq) / f_round_factor) * f_round_factor)
}
