#' Plot an annulus with different segments on an existing plot
#'
#' This function adds an annulus to an existing plot. It is very similar to the \code{\link[legendary]{annulus}} function except that the annulus can be composed of one or more "segments" with different formatting (color, etc.).
#' @param x Numeric, x-coordinate of center of annulus.
#' @param y Numeric, y-coordinate of center of annulus.
#' @param inner Numeric vector, inner radius of each segment.
#' @param outer Numeric vector, outer radius of each segment.
#' @param deg This is either:
#' \itemize{
#' 		\item{A numeric vector with one value per segment. The values will be standardized, then each segment will compose an arc with the number of degrees proportional to the value relative to the sum of values.
#' 		\item{A two-column matrix or data frame with numeric values. Each row pertains to one segment. The first column gives the starting degree for the segment and the second column the ending degree. Zero degrees is "north" and increasing values go clockwise.
#' 		\item{\code{NULL} (default), in which case each segment covers an equal number of degrees (same as the first option where each value is equal to the rest).
#' @param n Integer, number of vertices used to approximate a circle.
#' @param col Character or integer vector, colors used to fill segments.
#' @param density Numeric or numeric vector, the density of shading lines, in lines per inch. The default value of \code{NULL} causes no shading lines to be drawn. A zero value of density means no shading nor filling whereas negative values and \code{NA} suppress shading (and so allow color filling).
#' @param angle Numeric or numeric vector, the slope of shading lines, given as an angle in degrees (counter-clockwise).
#' @param border Character or integer vector, color used to draw segment borders.
#' @param force0 Logical, if \code{TRUE} then negative values of inner and outer are coerced to equal 0. If \code{FALSE} (default) then throws an error.
#' @param ... Arguments to send to \code{\link[graphics]{polygon}}.
#' @return Nothing (side-effect is to add a segmented annulus to an existing plot).
#' @seealso \code{\link[legendary]{annulus}}, \code{\link[graphics]{polygon}}
#' @examples
#' inner <- c(0.2, 0.4, 0.7)
#' outer <- c(0.5, 0.6, 0.71)
#' col <- c('red', 'blue', 'green')
#' par(pty='s')
#' plot(0, 0, xlim=c(-1, 1), ylim=c(-1, 1), col='white')
#' annulusSeg(x=0, y=0, inner, outer, col=col, border='black')
#' @export
annulusSeg <- function(
	x,
	y,
	inner,
	outer,
	deg=NULL,
	n=1000,
	col='black',
	density=NULL,
	angle=45,
	border=col,
	force0=FALSE,
	...
) {

	### catch errors
	if (length(inner) != length(outer)) {
		stop('Number of inner and outer radii must be equal (arguments "inner" and "outer").')
	}
	
	if (force0 & any(inner < 0)) {
		inner[inner < 0] <- 0
		warning('Forcing inner radius to 0 because <0.')
	}

	if (force0 & any(outer < 0)) {
		outer[outer < 0] <- 0
		warning('Forcing outer radius to 0 because <0.')
	}

	# repeat shorter vectors
	nSeg <- length(inner)
	
	nCol <- length(col)
	if (nCol < nSeg) {
		col <- c(col, rep(col, length.out=nSeg - nCol))
	}
	
	nBorder <- length(border)
	if (nBorder < nSeg) {
		border <- c(border, rep(border, length.out=nSeg - nBorder))
	}
	
	if (length(density) == 1) {
		density <- rep(density, nSeg)
	}

	if (length(angle) == 1) {
		angle <- rep(angle, nSeg)
	}

	# segment lengths
	if (is.null(deg)) deg <- rep(1, nSeg)
	if (!(class(deg) %in% c('matrix', 'data.frame'))) {
		
		degStd <- cumsum(deg) / sum(deg)
		degStart <- c(0, 360 * degStd[1:length(degStd) - 1])
		degEnd <- c(360 * degStd)
		deg <- matrix(c(degStart, degEnd), ncol=2, byrow=FALSE)
		
	}
		
		
	# draw segments
	for (countSeg in 1:nSeg) {
	
		thisInner <- inner[countSeg]
		thisOuter <- outer[countSeg]
		thisDeg <- deg[countSeg, ]
		thisCol <- col[countSeg]
		thisBorder <- border[countSeg]

		annulus(
			x=x,
			y=y,
			inner=thisInner,
			outer=thisOuter,
			deg=thisDeg,
			col=thisCol,
			border=thisBorder,
			n=n,
			force0=force0,
			density=density[countSeg],
			angle=angle[countSeg],
			...
		)
		
	}
	
}
