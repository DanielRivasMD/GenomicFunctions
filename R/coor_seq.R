
#coor_seq
#' @title
#' Coordinate Sequencer
#'
#' @description
#' Generates a vector given a vector of start coordinates 'f_start_coor' and a vector of end coordinates 'f_end_coor'
#'
#' @section Warning:
#'
#'
#' @param f_start_coor
#' Vector of start coordinates
#' @param f_end_coor
#' Vector of end coordinates
#' @return
#' Returns a vector sequence of concatenated integers in the same order as the coordinates
#' @export
#' @source
#'
#' @examples
#' x <- sample(1:100, 10)
#' y <- x + sample(1:10, 10, replace = T)
#' coor_seq(x, y)

coor_seq <- function(f_start_coor, f_end_coor){

	f_coor_length <- abs(f_end_coor)-abs(f_start_coor)+1
	f_dimension <- c(max(f_coor_length), length(f_coor_length))
	f_temp_mat <- matrix(data=NA, nrow=min(f_dimension), ncol=max(f_dimension))

	if(which.min(f_dimension)==1){												#long vector short interval

		for(f_peak in min(f_dimension):1){

			f_which_peak_height <- which(f_coor_length>=f_peak)
			f_temp_mat[f_peak, f_which_peak_height] <- f_start_coor[f_which_peak_height]+f_peak-1
		}
	}else{																		#long interval short vector

		f_temp_mat <- t(f_temp_mat)

		for(f_peak in min(f_dimension):1){

			f_temp_mat[, f_peak] <- c(f_start_coor[f_peak]:f_end_coor[f_peak], rep(NA, max(f_coor_length)-f_coor_length[f_peak]))
		}
	}

	f_peak_vec <- f_temp_mat[which(!is.na(f_temp_mat))]
	return(f_peak_vec)
}
