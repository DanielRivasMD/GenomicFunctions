
#' @title Tag Decoder
#'
#' @description
#' \emph{dec_tag} decodes a numeric vector given a specified position \emph{f_dec} by digits as \emph{d_numerator}
#'
#' @param f_seq Numerical vector
#' @param f_dec Position to decode
#' @param d_numerator Digit position
#'
#' @return Numerical vector of decoded values
#'
#' @examples
#'

	f_seq,
	f_dec,
	d_numerator = 100
) {

	f_seq <- (f_seq / f_dec / d_numerator) + 0.0000001
	return(floor((f_seq - floor(f_seq)) * d_numerator))
}
