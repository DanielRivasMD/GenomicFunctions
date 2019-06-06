
#dec_tag
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_seq
#'
#' @param f_dec
#'
#' @param f_numerator
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

dec_tag <- function(f_seq, f_dec, f_numerator = 100){

	f_seq <- (f_seq/f_dec/f_numerator)+0.0000001
	return(floor((f_seq-floor(f_seq))*f_numerator))
}
