
#' @title Hierarchical Cluster Tool
#'
#' @description
#' \emph{h_clust} provides hierarchical clustering for group(s) of populations, i. e. it calculates through \emph{peak_diff} and constructs a matrix of the absolute differences among populations
#' It recives a data.frame of populations as \emph{f_dataset} where a vector of column names to be compare must be specified as \emph{f_pop}
#'
#' @param f_dataset matrix to values
#' @param f_pop population names
#'
#' @return return
#'
#' @seealso \code{\link{locus_diff}}
#' @seealso \code{\link{peak_diff}}
#' @export

h_clust <- function(

	f_dataset,
	f_pop
) {

	f_n_pop <- length(f_pop)
	hcluster <- matrix(data = mapply(peak_diff, f_x = rep(1:f_n_pop, times = f_n_pop), f_y = rep(1:f_n_pop, each = f_n_pop), MoreArgs = list(f_data = f_dataset[, f_pop])), nrow = f_n_pop)
	diag(hcluster) <- NA
	colnames(hcluster) <- f_pop
	rownames(hcluster) <- f_pop

	return(hcluster)
}
