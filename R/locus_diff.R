
#' @title locus_diff
#'
#' @description
#' \emph{locus_diff}
#'
#' @param f_x f_x
#' @param f_y f_y
#' @param f_data f_data
#'
#' @return return
#'
#' @seealso \code{\link{peak_diff}}
#' @seealso \code{\link{h_clust}}
#' @export

locus_diff <- function(

	f_x,
	f_y,
	f_data
) {

	out_val <- sum(abs(f_data[, f_x] - f_data[, f_y]))
	return(out_val)
}
