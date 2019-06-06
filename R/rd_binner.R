
#rd_binner
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_chr
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

rd_binner <- function( f_chr ){

  depth_file <- data.table::fread(paste0(tmp_dir, tmp_file, "_cv_chr", f_chr, ".txt"), col.names = c('Chr', 'bp', 'rd'))
  out_val <- tibble::as.tibble(hit_df[hit_df$Chr == f_chr, "Chr"])
  tmp_ls <- list()
  for (over in 1:bin_overlaps){

    step_size <- bin_size * ((over - 1) / bin_overlaps)
    depth_file[, Positions := slid_win((depth_file[, .(bp)] + step_size), bin_size) - step_size]
    tmp_ls[[over]] <- aggregate(rd ~ Positions, data = depth_file, mean)
  }

  tmp_mean <- concat_ls(tmp_ls)
  out_val[, 2:3] <- tmp_mean[match(hit_df[hit_df$Chr == f_chr, ]$Positions, tmp_mean$Positions), ]
  return(out_val)
}
