
#rounder
#' @title
#' Rounder
#'
#' @description
#' Rounds numbers 'f_seq' to ceiling given a factor 'f_round_factor'
#'
#' @section Warning:
#'
#'
#' @param f_seq
#' Vector of numbers
#' @param f_round_factor
#' Rounding factor
#' @return
#' Returns a vector of numbers
#' @export
#' @source
#'
#' @examples
#' rounder(27, 10)
#' rounder(341:710, 20)

rounder <- function(f_seq, f_round_factor){

	return(ceiling((f_seq)/f_round_factor)*f_round_factor)
}
