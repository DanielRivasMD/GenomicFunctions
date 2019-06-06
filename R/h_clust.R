
#h_clustering
#' @title
#'
#'
#' @description
#'
#'
#' @section Warning:
#'
#'
#' @param f_dataset
#'
#' @param f_pop
#'
#' @return
#'
#' @export
#' @source
#'
#' @examples
#'

h_clust <- function(f_dataset, f_pop){

	f_n_pop <- length(f_pop)
	hcluster <- matrix(data=mapply(peak_diff, f_x=rep(1:f_n_pop, times=f_n_pop), f_y=rep(1:f_n_pop, each=f_n_pop), MoreArgs=list(f_data=f_dataset[, f_pop])), nrow=f_n_pop)
	diag(hcluster) <- NA
	colnames(hcluster) <- f_pop
	rownames(hcluster) <- f_pop
	return(hcluster)

	# h_clust provides h-clustering for group(s) of populations, i. e. it calculates through "peak_diff" and constructs a matrix of the absolute differences among populations.
	# It recives a data.frame of populations (f_dataset) where a vector of column names to be compare must be specified (f_pop).
}
