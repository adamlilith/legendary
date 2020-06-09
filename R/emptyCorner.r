#' Find the corner of a plot region that is most empty of plot elements
#'
#' This function identifies the corner of a plot region that is the least occupied by plot elements.
#' @param x A two-column matrix or data frame with points representing plot items.
#' @param by Character, method by which to identify the "most empty" quadrat. Any of (partial matching is OK, and case is ignored):
#' \itemize{
#' 		\item \code{'distance'} (default): Find the corner that has the greatest distance between it and the closest point.
#' 		\item \code{'density'}: Find the corner that has the least density of points.
#' 		\item \code{'dd'}: Find the corner that has the least density of closest points.
#' }
#' @param onlyIn Logical, if \code{TRUE} (default), only consider plot items that are in the plot region.
#' @param least Logical, if \code{TRUE} (default), then indicate the least-occupied corner. If \code{FALSE} then indicate the most-occupied corner.
#' @details Note that you must already have a plot made for this to work.
#' @return Integer: \code{1}: top left; \code{2}: top right; \code{3}: bottom left; \code{4}: bottom right.
#' @examples
#' # generate some numbers
#' set.seed(123)
#' x <- c(4, rnorm(100), runif(200, -2, 0))
#' y <- c(2, rnorm(100), runif(200, -2, 0))
#' xlim <- c(min(x), 1.5 * max(x))
#' plot(x, y, xlim=xlim)
#' xy <- cbind(x, y)
#' emptyCorner(xy)
#' emptyCorner(xy, by='density')
#' emptyCorner(xy, by='dd')
#' @export
emptyCorner <- function(
	x,
	by = 'distance',
	onlyIn = TRUE,
	least = TRUE
) {

	usr <- par('usr') # plot corners
	x <- cbind(x) # catches cases with just one point
	
	# truncate items if considering items only in plot region
	if (onlyIn) {
	
		bads <- which(x[ , 1] < usr[1] | x[ , 1] > usr[2] | x[ , 2] < usr[3] | x[ , 2] > usr[4])
		if (length(bads) > 0) x <- x[-bads, ]
		
	}
	
	# find most empty corner
	if (nrow(x) > 0) {

		# corner coordinates
		ll <- usr[c(1, 3)]
		lr <- usr[c(2, 3)]
		ul <- usr[c(1, 4)]
		ur <- usr[c(2, 4)]
		
		# what shall we do?
		by <- tolower(by)
		options <- c('distance', 'density', 'dd')
		doThis <- options[pmatch(by, options)]
	
		# fine emptiest corner
		if (doThis == 'distance') {
		
			llDens <- -1 * min(sqrt((ll[1] - x[ , 1])^2 + (ll[2] - x[ , 2])^2))
			lrDens <- -1 * min(sqrt((lr[1] - x[ , 1])^2 + (lr[2] - x[ , 2])^2))
			ulDens <- -1 * min(sqrt((ul[1] - x[ , 1])^2 + (ul[2] - x[ , 2])^2))
			urDens <- -1 * min(sqrt((ur[1] - x[ , 1])^2 + (ur[2] - x[ , 2])^2))
		
		} else if (doThis == 'density') {
		
			xMid <- usr[1] + (usr[2] - usr[1]) / 2
			yMid <- usr[3] + (usr[4] - usr[3]) / 2
			
			llDens <- sum(x[ , 1] <= xMid & x[ , 2] <= yMid)
			lrDens <- sum(x[ , 1] > xMid & x[ , 2] <= yMid)
			ulDens <- sum(x[ , 1] <= xMid & x[ , 2] > yMid)
			urDens <- sum(x[ , 1] > xMid & x[ , 2] > yMid)

		} else if (doThis == 'dd') {
	
			llDens <- -1 * sum((ll[1] - x[ , 1])^2 + (ll[2] - x[ , 2])^2)
			lrDens <- -1 * sum((lr[1] - x[ , 1])^2 + (lr[2] - x[ , 2])^2)
			ulDens <- -1 * sum((ul[1] - x[ , 1])^2 + (ul[2] - x[ , 2])^2)
			urDens <- -1 * sum((ur[1] - x[ , 1])^2 + (ur[2] - x[ , 2])^2)
	
		}
	
		dens <- c(ulDens, urDens, llDens, lrDens)
		out <- if (least) {
			which.min(dens)
		} else {
			which.max(dens)
		}

	} else {
		out <- NA
	}
	
	out
	
}

