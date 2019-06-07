
#' @title class_strrugle_to_character
#'
#' @description
#' \emph{class_strrugle_to_character}
#'
#' @param f_df f_df
#' @param f_class f_class
#'
#' @return return
#'
#' @seealso \code{\link{class_strrugle_to_numeric}}
#' @export

class_strrugle_to_character <- function(

	f_df,
	f_class
) {

	for(i in which(sapply(f_df, class) == f_class)) f_df[[i]] = as.character(f_df[[i]])
	return(f_df)
}
