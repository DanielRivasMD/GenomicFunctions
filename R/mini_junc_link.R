
#' @title Mini Junction Linker
#'
#' @description
#' \emph{mini_junc_link}
#'
#' @param f_df data.frame
#' @param f_upp_col Column interpreted as upstream
#' @param f_down_col Column interpreted as downstream
#' @param f_pos_col Positions column
#' @param f_thres Threshold
#'
#' @return return
#'
#' @export

mini_junc_link <- function(

	f_df,
	f_upp_col,
	f_down_col,
	f_pos_col,
	f_thres = 1
) {

	f_tmp_df <- f_df
	f_tmp_df <- f_tmp_df[which(f_tmp_df[, f_upp_col] >= 1 | f_tmp_df[, f_down_col] >= 1), c(f_pos_col, f_upp_col, f_down_col)]
	f_tmp_ed <- f_tmp_df[which(f_tmp_df[, f_upp_col] >= f_thres) + 1, ]
	f_vec_ed <- f_tmp_ed[which(f_tmp_ed[, f_upp_col] < f_thres), f_pos_col]
	f_vec_eu <- f_tmp_ed[which(f_tmp_ed[, f_upp_col] < f_thres) - 1, f_pos_col]
	f_down_pos <- match(f_vec_ed, f_df[, f_pos_col])
	f_up_pos <- match(f_vec_eu, f_df[, f_pos_col])

	f_df_e <- f_df[c(rbind(f_up_pos, f_down_pos)), c(f_pos_col, f_upp_col, f_down_col)]
	f_df_e[, "dist"] <- c(f_df_e[-1, f_pos_col], f_df_e[dim(f_df_e)[1], f_pos_col]) - f_df_e[, f_pos_col]
	f_df_e[which((1:(dim(f_df_e)[1]) %% 2) == 0), "dist"] <- NA

	return(f_df_e)
}
