#' Add a legend with thresholded values to a plot
#'
#' This function adds a legend to an existing plot that shows blocks of color. It is useful, for example, for displaying maps of continuous values that have been thresholded at multiple values. It first draws a "containing" box then inside the box a second box with a set of stacked bars, one per color. A legend title and labels for levels indicated by the color bar can be added.
#' @param x Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the top left of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'}, \code{'bottomright'}, \code{'top'}, \code{'bottom'}, \code{'left'}, \code{'right'}, or \code{'center'}).
#' @param y Numeric or \code{NULL}.
#' @param inset Numeric. If \code{x} is a word describing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertical offset.
#' @param width Numeric. Scaling factor for box width.
#' @param height Numeric. Scaling factor for box height.
#' @param labels Vector of characters of numeric values. Labels (from least to most) of levels of the focal variable indicated by the color ramp.
#' @param labAdjX Numeric between 0 and 1. Horizontal position of labels relative to the containing box.
#' @param labAdjY Vectors of numbers typically between 0 and 1 indicating relative position of \code{labels} along the color bar. The bottommost value is at 0 and topmost at 1.
#' @param col List of characters or integers. Names of colors. The first color will be the lowest value and the last the highest value.
#' @param colBorder Characters or integer. Names of color to be used to draw an outline around each color in the color box. Use \code{NA} (default) to skip drawing an outline.
#' @param border Character or integer. Name (or integer code) of color to use to draw border of the color bar.
#' @param title Character or \code{NULL}. Name of title for the legend.
#' @param titleAdj Two numeric values between 0 and 1. Position of the legend relative to the container box. The first pertains to horizontal positioning and the second vertical positioning.
#' @param adjX Two numeric values between 0 and 1. Size of the color bar in the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.
#' @param adjY Two numeric values between 0 and 1. Size of the color bar in the y-dimension as a proportion of the container box size. The first pertains to the bottom of the bar and the second the top.
#' @param boxBg Character or integer. Name (or integer code) of color to use to use for box containing legend. Leave as \code{NULL} to not draw a box.
#' @param boxBorder Character or integer. Name (or integer code) of color to use to use for box border containing legend. Leave as \code{NULL} to not draw a box border.
#' @param swatches A list or lists, each of which contains information on extra "swatches" of a single color to add above/below/on the color bar. These are useful, for example, for describing data that does not fall into the range covered by the data (e.g., \code{NA}'s).  If \code{swatches} is \code{NULL} then it is ignored.  Otherwise, it is a list of lists, and each sublist defines a different swatch. Sublists have these elements:
#' \itemize{
#' 	\item \code{swatchAdjY} Two numeric values between 0 and 1. Size of the swatches the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.  (Swatches will always be left-right aligned with the color bar.)
#' 	\item \code{col} Character or integer representing the swatch's color.
#' 	\item \code{border} Character or integer. Name (or integer code) of color to use to draw border of the swatch.
#' 	\item \code{labels} Character, for labelling the swatch.
#' }
#' @param ... Arguments to pass to \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}}, or \code{\link[graphics]{text}}.
#' @return Nothing (side effect is to add a legend to an existing graphics device).
#' @seealso \code{\link[graphics]{legend}}, \code{\link[legendary]{legendQuad}}
#' @examples
#' wealth <- data.frame(
#' 	country = c('USA', 'Japan', 'Malaysia', 'Germany', 'England',
#' 'North Korea', 'South Korea', 'Nigeria'),
#' 	pop = c(325365189, 126672000, 31718000, 82175700,
 #' 54786300, 25370000, 51446201, 185989640),
#' 	gdp = c(18558, 5420, 913.593, 4150, 1870, 12.38, 2029, 1166),
#' 	perCapGdp = c(145894, 57220, 28490, 50206, 34205, 583, 39446, 6351),
#' 	hivPerc = c(0.03, 0.01, 0.40, 0.15, 0.16, NA, 0.29, 0.29)
#' )
#'
#' # color categories
#' hivPerc <- round(100 * wealth$hivPerc / max(wealth$hivPerc, na.rm=TRUE))
#' cols <- rep(NA, length(hivPerc))
#' cols[hivPerc <= 33] <- 'lightsalmon'
#' cols[hivPerc > 33 & hivPerc <= 67] <- 'red'
#' cols[hivPerc > 67] <- 'darkred'
#' cols[is.na(hivPerc)] <- 'gray'
#' 
#' # rescale population (symbols size)
#' popRescaled <- 1 + 3 * (log10(wealth$pop) - min(log10(wealth$pop)))
#' plot(wealth$gdp, wealth$perCapGdp, pch=21,
#' cex=popRescaled, bg=cols, xlab='GDP (Billion $)', ylab='GDP Per Capita ($)')
#' text(wealth$gdp, wealth$perCapGdp, labels=as.character(wealth$country),
#' pos=4, xpd=NA)
#' 
#' legendBreaks(
#' 	x='bottomright',
#' 	y = NULL,
#' 	inset = 0.02,
#' 	width = 0.23,
#' 	height = 0.7,
#' 	labels = c(33, 67),
#' 	labAdjX = 0.6,
#' 	labAdjY = c(0.33, 0.67),
#' 	col = c('lightsalmon', 'red', 'darkred'),
#' 	border = 'black',
#' 	title = 'HIV (%)',
#' 	titleAdj = c(0.5, 0.9),
#' 	adjX = c(0.2, 0.5),
#' 	adjY = c(0.2, 0.8),
#' 	boxBorder = 'black',
#' 	swatches=list(
#' 		list(
#' 			swatchAdjY=c(0.05, 0.15),
#' 			col='gray',
#' 			border='black',
#' 			labels='NA'
#' 		)
#' 	)
#' )
#' @export

legendBreaks <- function(
	x,
	y = NULL,
	inset = 0,
	width = 0.2,
	height = 0.5,
	labels = c(0.25, 0.50, 0.75),
	labAdjX = 0.75,
	labAdjY = c(0.25, 0.5, 0.75),
	col = c('gray', 'yellow', 'orange', 'red'),
	colBorder = NA,
	border = 'black',
	title = 'Title',
	titleAdj = c(0.5, 0.9),
	adjX = c(0.2, 0.5),
	adjY = c(0.1, 0.8),
	boxBg = par('bg'),
	boxBorder = 'black',
	swatches = NULL,
	...
) {

	# catch errors
	if (!is.null(labels)) if (length(labels) != length(labAdjY)) stop('Length of argument "labels" must be same as length of "labAdjY".')

	# number of color blocks
	numCols <- length(col)
	
	# get coordinate stats for existing plot
	pos <- par('usr')

	plotWidth <- pos[2] - pos[1]
	plotHeight <- pos[4] - pos[3]

	legWidth <- width * plotWidth
	legHeight <- height * plotHeight

	# get containing box location top left coordinate
	if (class(x) == 'character') {

		if (length(inset) == 1) inset <- c(inset, inset)

		xInset <- inset[1] * plotWidth
		yInset <- inset[2] * plotHeight

		if (x == 'topleft') {
			x <- pos[1] + xInset
			y <- pos[4] - yInset
		} else if (x == 'topright') {
			x <- pos[2] - legWidth - xInset
			y <- pos[4] - yInset
		} else if (x == 'bottomleft') {
			x <- pos[1] + xInset
			y <- pos[3] + legHeight + yInset
		} else if (x == 'bottomright') {
			x <- pos[2] - legWidth - xInset
			y <- pos[3] + legHeight + yInset
		} else if (x == 'bottom') {
			x <- pos[1] + 0.5 * plotWidth - 0.5 * legWidth
			y <- pos[3] + legHeight + yInset
		} else if (x == 'top') {
			x <- pos[1] + 0.5 * plotWidth - 0.5 * legWidth
			y <- pos[4] - yInset
		} else if (x == 'left') {
			x <- pos[1] + xInset
			y <- pos[3] + 0.5 * plotHeight + 0.5 * legHeight
		} else if (x == 'right') {
			x <- pos[2] - xInset - legWidth
			y <- pos[3] + 0.5 * plotHeight + 0.5 * legHeight
		} else if (x == 'center') {
			x <- pos[1] + 0.5 * (pos[2] - pos[1]) - 0.5 * legWidth
			y <- pos[3] + 0.5 * plotHeight + 0.5 * legHeight
		} else {
			error('The "x" coordinate in function "legendGrad" must be a numeric value or\nan accepted position word (e.g., "top", "topleft", "bottomright", etc.).')
		}

	}

	# draw containing box
	if (!is.null(boxBorder)) graphics::polygon(c(x, x + width * plotWidth, x + width * plotWidth, x), c(y, y, y - height * plotHeight, y - height * plotHeight), col=boxBg, border=boxBorder, xpd=NA, ...)

	# legend title
	graphics::text(x + titleAdj[1] * legWidth, y - (1 - titleAdj[2]) * legHeight, labels=title, xpd=NA, ...)

	# get color bounding box
	left <- x + adjX[1] * legWidth
	right <- x + adjX[2] * legWidth
	top <- y - (1 - adjY[2]) * legHeight
	bottom <- y - (1 - adjY[1]) * legHeight

	# get each patch's box dimensions
	blockHeight <- top - bottom
	eachBlockHeight <- blockHeight / numCols

	# plot each patch
	for (countCol in seq_along(col)) {
	
		thisCol <- col[countCol]
		thisBottom <- bottom + eachBlockHeight * (countCol - 1)
		thisTop <- bottom + eachBlockHeight * countCol

		xs <- c(left, right, right, left)
		ys <- c(thisBottom, thisBottom, thisTop, thisTop)
		
		graphics::polygon(
			x=xs,
			y=ys,
			col=thisCol,
			border=colBorder,
			xpd=NA,
			...
		)
		
	} # next color box

	# border around all colors
	if (!is.na(border)) graphics::polygon(c(left, right, right, left), c(bottom, bottom, top, top), col=NA, border=border, xpd=NA, ...)

	# add labels
	if (!is.null(labels)) {

		labY <- bottom + labAdjY * (top - bottom)
		text(x + legWidth * rep(labAdjX, length(labels)), labY, labels=labels, pos=4, xpd=NA, ...)
		
	}

	# add swatches
	if (!is.null(swatches)) {

		for (i in seq_along(swatches)) {

			top <- y - (1 - swatches[[i]]$swatchAdjY[1]) * legHeight
			bottom <- y - (1 - swatches[[i]]$swatchAdjY[2]) * legHeight

			graphics::polygon(c(left, right, right, left), c(bottom, bottom, top, top), col=swatches[[i]]$col, border=swatches[[i]]$border, ...)
			labY <- top <- y - (1 - (mean(c(swatches[[i]]$swatchAdjY[[1]], swatches[[i]]$swatchAdjY[[2]])))) * legHeight
			text(x + legWidth * labAdjX, labY, labels=swatches[[i]]$labels, pos=4, xpd=NA, ...)

		}

	}

}
