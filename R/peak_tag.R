
#peak_tag
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_df_pos
#'
#' @param f_seq_dist
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

peak_tag <- function(f_df_pos, f_seq_dist){

	out_val <- data.frame(max_ix=which(c(f_df_pos[-1], 0)-f_df_pos!=f_seq_dist))
	out_val$min_ix <- c(1, out_val$max_ix[-length(out_val$max_ix)]+1)
	return(out_val)
}
