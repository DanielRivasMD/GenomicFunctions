
#tb_collector
#' @title
#'
#'
#' @description
#' Collects vector data into a table of standard format defined by expected values
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

tb_collector <- function(f_data, f_ix, f_exp_val_range, f_val=NULL){

	# EMPTY TABLE FOR F_OUT
	f_out <- table(f_exp_val_range) - 1

	# CONTROL SWITCH
	f_range_control <- 0

	for(f_i in f_ix){

		if(!is.null(f_val)){

			if("data.frame" %in% class(f_data[[f_i]])){

				f_i_tmp_tb <- table(f_data[[f_i]][, f_val])
			}else if(f_val == 'vector'){

				f_i_tmp_tb <- table(f_data[[f_i]])
			}
		}else{

			f_i_tmp_tb <- f_data[[f_i]]
		}

		if(length(f_i_tmp_tb) > length(f_out)){

			f_range_control <- f_range_control + 1
		}
		f_tb_match <- match(names(f_i_tmp_tb), names(f_out))
		f_out[f_tb_match[!is.na(f_tb_match)]] <- f_out[f_tb_match[!is.na(f_tb_match)]] + f_i_tmp_tb[!is.na(f_tb_match)]
	}

	if(f_range_control > 0){

		write(paste0('Values out of range ', f_range_control, ' - Never tell me the odds'), stdout())
	}

	return(f_out)
}
