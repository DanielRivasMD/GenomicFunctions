
#' @title peak_diff
#'
#' @description
#' \emph{peak_diff}
#'
#' @inheritParams locus_diff
#'
#' @return return
#'
#' @seealso \code{\link{locus_diff}}
#' @seealso \code{\link{h_clust}}
#' @seealso \code{\link{peak_iden}}
#' @seealso \code{\link{unique_coor}}
#' @seealso \code{\link{shared_coor}}
#' @export

peak_diff <- function(

	f_x,
	f_y,
	f_data
) {

	p_x <- peak_iden(f_data[, f_x])
	p_y <- peak_iden(f_data[, f_y])
	u_data <- unique_coor(p_x[, c("lower_lim_ix", "upper_lim_ix")], p_y[, c("lower_lim_ix", "upper_lim_ix")], "p_x", "p_y")
	out_val <- dim(u_data$p_x)[1] + dim(u_data$p_y)[1]

	return(out_val)
}
