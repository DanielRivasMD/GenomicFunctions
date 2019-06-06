
#slid_win
#' @title
#' Window Slider
#'
#' @description
#' Takes in a vector of numbers 'f_seq' and outputs its positions assuming 'f_bin_size'
#'
#' @section Warning:
#'
#'
#' @param f_seq
#' Vector of numbers
#' @param f_bin_size
#' Bin size
#' @return
#' Returns a vector of numbers
#' @export
#' @source
#'
#' @examples
#' slid_win(37, 5)
#' slid_win(1:100, 15)

slid_win <- function(f_seq, f_bin_size=bin_size){

	return(ceiling(((f_seq)-(f_bin_size/2))/f_bin_size)*f_bin_size)
}
