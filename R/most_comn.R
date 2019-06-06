
#most_comn
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_x
#'
#' @param na_rm
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

most_comn <- function(f_x, na_rm = F){

	if(na_rm == T){

		f_x <- f_x[!is.na(f_x)]
	}

	f_ux <- unique(f_x)
	f_ux[which.max(tabulate(match(f_x, f_ux)))]
}
