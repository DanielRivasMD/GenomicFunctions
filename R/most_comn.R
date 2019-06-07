
#' @title Most Common
#'
#' @description
#' \emph{most_comn}
#'
#' @param f_x input
#' @param na_rm boolean
#'
#' @return return
#'
#' @export

most_comn <- function(

	f_x,
	na_rm = FALSE
) {

	if(na_rm == TRUE){

		f_x <- f_x[!is.na(f_x)]
	}

	f_ux <- unique(f_x)
	f_ux[which.max(tabulate(match(f_x, f_ux)))]
}
