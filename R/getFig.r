#' Return coordinates for par(fig=xxxx)
#'
#' This function can be used with the \code{\link[graphics]{layout}} function for defining the positioning of multiple plots on a single plotting device. Given a matrix used in the \code{layout} function and the number indicating which a particular subplot area, it returns the min/max x- and y-coordinates for the subplot relative to the entire plottig region.
#' @param layout Matrix representing the positioning of subplots. See \code{\link[graphics]{layout}}.
#' @param n Integer, indicating which suboplot to plot (corresponds to value(s) in \code{layout}).
#' @return A four-element numeric vector.
#' @examples
#' lo <- matrix(c(1, 2, 3), nrow=1)
#' layout(lo)
#' subplot1 <- getFig(lo, 1)
#' subplot2 <- getFig(lo, 2)
#' subplot3 <- getFig(lo, 3)
#' par(fig=subplot2, new=FALSE)
#' plot(1, 1, main='Subplot 2')
#' par(fig=subplot1, new=TRUE)
#' plot(1, 1, main='Subplot 1')
#' par(fig=subplot3, new=TRUE)
#' plot(1, 1, main='Subplot 3')
#' @export

getFig <- function(layout, n) {

	# convert layout to binary
	layout <- layout==n
	
	# get min/max column/row
	minCol <- min(which(colSums(layout) > 0))
	maxCol <- max(which(colSums(layout) > 0))
	minRow <- min(which(rowSums(layout) > 0))
	maxRow <- max(which(rowSums(layout) > 0))

	# border values
	fig <- c(
		(minCol - 1) / ncol(layout),
		maxCol / ncol(layout),
		1 - maxRow / nrow(layout),
		1 - (minRow - 1) / nrow(layout)
	)
	
	names(fig) <- c('xmin', 'xmax', 'ymin', 'ymax')
	fig

}


