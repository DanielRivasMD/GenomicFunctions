
#concat_ls
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_data
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

concat_ls <- function(f_data){

	if("data.frame" %in% class(f_data[[1]]) | "matrix" %in% class(f_data[[1]])){

		for(f_looping in 1:length(f_data)){

			if(f_looping==1){

				f_array <- f_data[[f_looping]]
			}else{

				if(dim(f_data[[f_looping]])[1]>0){

					f_array[(1:dim(f_data[[f_looping]])[1])+dim(f_array)[1], ] <- f_data[[f_looping]]
				}
			}
		}
	}else{

		for(f_looping in 1:length(f_data)){

			if(f_looping==1){

				if(class(f_data[[f_looping]])=="table"){

					f_names <- names(f_data[[f_looping]])
				}
				f_array <- f_data[[f_looping]]

			}else{

				f_array[seq_along(f_data[[f_looping]])+length(f_array)] <- f_data[[f_looping]]

				if(class(f_data[[f_looping]])=="table"){

					f_names[seq_along(f_data[[f_looping]])+length(f_names)] <- names(f_data[[f_looping]])
				}
			}
		}

		if(class(f_data[[f_looping]])=="table"){

			names(f_array) <- f_names
		}
	}

	return(f_array)
}
