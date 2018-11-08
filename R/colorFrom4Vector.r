#' Transforms a vector of four values to a set of HSV color values and their position on a quad plot.
#'
#' This function converts a vector of four values to HSV color values for plotting. It is useful for showing, for example, the relative probabilities of an individual assigned to four populations.
#' @param x Numeric vector with four elements. The values should be proportions or represent proportions (i.e., they will be standardized so they sum to 1).
#' @param cols Four-element vector of characters or integers. Names (or integer codes) of four colors used to convert the value \code{x} to HSV color format. The first color corresponds to the condition where the first value of \code{x} is >0 and the remainder = 0. The second represents the condition where the second value of \code{x} is >0 and the others = 0, and so on.
#' @return A list with two named elements: \code{col}, an HSV color code translation for the values of \code{x} and \code{xy}, a two-element numeric vector with the coordinates of \code{x} if plotted such that \code{col[1]} is at coordinate \code{(0, 0)}, \code{col[2]} at \code{(0, 1)}, \code{col[3]} at \code{(1, 1)}, and \code{col[4]} at \code{(1, 0)}.
#' @seealso \code{\link[grDevices]{hsv}}, \code{\link[IDPmisc]{col2hsv}}
#' @examples
#' plot(0, xlim=c(0, 1), ylim=c(0, 1), xlab='<--- Population A     Population D --->', ylab='<--- Population A     Population B --->')
#' for (i in 1:100) {
#' 	x <- runif(4)^5
#' 	out <- colorFrom4Vector(x, cols = c('cyan', 'magenta', 'yellow', 'black'))
#' 	points(out$xy[1], out$xy[2], col=hsv(out$col[1], out$col[2], out$col[3]), pch=16)
#' }
#' @export
colorFrom4Vector <- function(
	x,
	cols = c('white', 'cyan', 'black', 'red')
) {

	if (anyNA(x)) stop('NA values are not allowed in "x" in function "colFrom4Vector()."')
	if (anyNA(cols)) stop('NA values are not allowed in "cols" in function "colFrom4Vector().".')
	if (!(class(x) %in% c('integer', 'numeric'))) stop('The values in "x" must be of class "numeric" or "integer" in function "colFrom4Vector()."')
	if (length(x) != 4) stop('The vector "x" must contain four values in function "colFrom4Vector()."')
	if (any(x < 0)) stop('All values in "x" must be >= 0 in function "colFrom4Vector()."')

	x <- x / sum(x)
	x <- rbind(x)
	
	# coordinates
	xx <- sum(x * c(0, 0, 1, 1))
	yy <- sum(x * c(0, 1, 1, 0))
	xy <- c(x=xx, y=yy)

	# color
	colsMat <- IDPmisc::col2hsv(cols)
	colnames(colsMat) <- c('LL', 'UL', 'UR', 'LR')
	rownames(colsMat)[1:3] <- c('h', 's', 'v')
	if (nrow(colsMat) == 4) rownames(colsMat)[4] <- 'a'
	
	h <- (1 - xx) * (colsMat['h', 'UL'] * yy + colsMat['h', 'LL'] * (1 - yy)) + xx * (colsMat['h', 'UR'] * yy + colsMat['h', 'LR'] * (1 - yy))
	s <- (1 - xx) * (colsMat['s', 'UL'] * yy + colsMat['s', 'LL'] * (1 - yy)) + xx * (colsMat['s', 'UR'] * yy + colsMat['s', 'LR'] * (1 - yy))
	v <- (1 - xx) * (colsMat['v', 'UL'] * yy + colsMat['v', 'LL'] * (1 - yy)) + xx * (colsMat['v', 'UR'] * yy + colsMat['v', 'LR'] * (1 - yy))
	col <- c(h, s, v)
	names(col) <- c('h', 's', 'v')
	
	if (nrow(colsMat) == 4) {
		a <- xx * (colsMat['a', 'UL'] * yy + colsMat['a', 'LL'] * (1 - yy)) + (1 - xx) * (colsMat['a', 'UR'] * yy + colsMat['a', 'LR'] * (1 - yy))
		col <- c(col, a)
		names(col)[4] <- 'a'
	}

	out <- list(col=col, xy=xy)
	out

}
