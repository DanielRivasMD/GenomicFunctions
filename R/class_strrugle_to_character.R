
#class_strrugle_to_character
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_df
#'
#' @param f_class
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

class_strrugle_to_character <- function(f_df, f_class){
  for(i in which(sapply(f_df, class) == f_class)) f_df[[i]] = as.character(f_df[[i]])
  return(f_df)
}
