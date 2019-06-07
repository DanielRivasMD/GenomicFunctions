
#' @title class_strrugle_to_numeric
#'
#' @description
#' \emph{class_strrugle_to_numeric}
#'
#' @inheritParams class_strrugle_to_character
#'
#' @return return
#'
#' @seealso \code{\link{class_strrugle_to_character}}
#' @export

class_strrugle_to_numeric <- function(

	f_df,
	f_class
) {

	for(i in which(sapply(f_df, class) == f_class)) f_df[[i]] = as.numeric(f_df[[i]])
	return(f_df)
}
