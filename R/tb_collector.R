
#' @title tb_collector
#'
#' @description
#' \emph{tb_collector} collects vector data into a table of standard format defined by expected values
#'
#' @param f_data f_data
#' @param f_ix f_ix
#' @param f_exp_val_range expected table range
#' @param f_val f_val
#'
#' @return return
#'
#' @seealso \code{\link{concat_ls}}
#' @export

tb_collector <- function(

	f_data,
	f_ix,
	f_exp_val_range,
	f_val = NULL
) {

	# empty table for f_out
	f_out <- table(f_exp_val_range) - 1

	# control switch
	f_range_control <- 0

	for(f_i in f_ix){
		#
		if(!is.null(f_val)){
			#
			if("data.frame" %in% class(f_data[[f_i]])){
				#
				f_i_tmp_tb <- table(f_data[[f_i]][, f_val])
			}else if(f_val == 'vector'){
				#
				f_i_tmp_tb <- table(f_data[[f_i]])
			}
		}else{
			#
			f_i_tmp_tb <- f_data[[f_i]]
		}
		#
		if(length(f_i_tmp_tb) > length(f_out)){
			#
			f_range_control <- f_range_control + 1
		}
		f_tb_match <- match(names(f_i_tmp_tb), names(f_out))
		f_out[f_tb_match[!is.na(f_tb_match)]] <- f_out[f_tb_match[!is.na(f_tb_match)]] + f_i_tmp_tb[!is.na(f_tb_match)]
	}

	if(f_range_control > 0){
		#
		cat(paste0('Values out of range ', f_range_control, ' - Never tell me the odds'))
	}

	return(f_out)
}
