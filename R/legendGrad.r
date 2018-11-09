#' Adds a gradient legend to a plot
#'
#' This function adds a legend to an existing plot that shows a gradient in color. It first draws a "containing" box then a bar with a color gradient inside the box. A legend title and labels for levels indicated by the color bar can be added.
#' @param x Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the top left of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'}, \code{'bottomright'}, \code{'top'}, \code{'bottom'}, \code{'left'}, or \code{'right'}).
#' @param y Numeric or \code{NULL}.
#' @param inset Numeric. If \code{x} is a word describing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertical offset.
#' @param width Numeric. Scaling factor for box width.
#' @param height Numeric. Scaling factor for box height.
#' @param labels Vector of characters of numeric values. Labels (from least to most) of levels of the focal variable indicated by the color ramp.
#' @param labAdj Numeric between 0 and 1. Position of labels relative to the containing box.
#' @param col List of characters or integers. Names of colors to be used to create a gradient to fill the legend bar. The first color will be the lowest value and the last the highest value.
#' @param border Character or integer. Name (or integer code) of color to use to draw border of the gradient bar.
#' @param title Character or \code{NULL}. Name of title for the legend.
#' @param titleAdj Two numeric values between 0 and 1. Position of the legend relative to the container box. The first pertains to horizontal positioning and the second vertical positioning.
#' @param gradAdjX Two numeric values between 0 and 1. Size of the gradient bar in the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.
#' @param gradAdjY Two numeric values between 0 and 1. Size of the gradient bar in the y-dimension as a proportion of the container box size. The first pertains to the bottom of the bar and the second the top.
#' @param boxBg Character or integer. Name (or integer code) of color to use to use for box containing legend. Leave as \code{NULL} to not draw a box.
#' @param boxBorder Character or integer. Name (or integer code) of color to use to use for box border containing legend. Leave as \code{NULL} to not draw a box border.
#' @param swatches A list or lists, each of which contains information on extra "swatches" of a single color to add above/below/on the gradient bar. These are useful, for example, for describing data that does not fall into the range covered by the data (e.g., \code{NA}'s).  If \code{swatches} is \code{NULL} then it is ignored.  Otherwise, it is a list of lists, and each sublist defines a different swatch. Sublists have these elements:
#' \itemize{
#' 	\item \code{swatchAdjY} Two numeric values between 0 and 1. Size of the swatches the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.  (Swatches will always be left-right aligned with the gradient bar.)
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
#' # color ramp
#' colFx <- grDevices::colorRampPalette(c('white', 'red'))
#' cols <- colFx(100)
#' hivPerc <- round(100 * wealth$hivPerc / max(wealth$hivPerc, na.rm=TRUE))
#' cols <- cols[hivPerc]
#' cols[is.na(cols)] <- 'gray'
#'
#' # rescale population (symbols size)
#' popRescaled <- 1 + 3 * (log10(wealth$pop) - min(log10(wealth$pop)))
#' plot(wealth$gdp, wealth$perCapGdp, pch=21,
#' cex=popRescaled, bg=cols, xlab='GDP (Billion $)', ylab='GDP Per Capita ($)')
#' text(wealth$gdp, wealth$perCapGdp, labels=as.character(wealth$country),
#' pos=4, xpd=NA)
#'
#' legendGrad(
#' 	x='bottomright',
#' 	y = NULL,
#' 	inset = 0.02,
#' 	width = 0.23,
#' 	height = 0.3,
#' 	labels = 100 * pretty(wealth$hivPerc),
#' 	labAdj = 0.6,
#' 	col = c('white', 'red'),
#' 	border = 'black',
#' 	title = 'HIV (%)',
#' 	titleAdj = c(0.5, 0.9),
#' 	gradAdjX = c(0.2, 0.5),
#' 	gradAdjY = c(0.2, 0.8),
#' 	boxBorder = 'black',
#' 	swatches=list(
#' 		list(
#' 			swatchAdjY=c(0.85, 0.95),
#' 			col='gray',
#' 			border='black',
#' 			labels='NA'
#' 		)
#' 	)
#'
#' )
#' @export

legendGrad <- function(
	x,
	y = NULL,
	inset = 0,
	width = 0.2,
	height = 0.5,
	labels = NULL,
	labAdj = 0.75,
	col = c('yellow', 'orange', 'red'),
	border = 'black',
	title = 'Title',
	titleAdj = c(0.5, 0.9),
	gradAdjX = c(0.2, 0.5),
	gradAdjY = c(0.1, 0.8),
	boxBg = par('bg'),
	boxBorder = 'black',
	swatches = NULL,
	...
) {

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
		} else {
			error('The "x" coordinate in function "legendGrad" must be a numeric value or\nan accepted position word (e.g., "top", "topleft", "bottomright", etc.).')
		}

	}

	# draw containing box
	if (!is.null(boxBorder)) graphics::polygon(c(x, x + width * plotWidth, x + width * plotWidth, x), c(y, y, y - height * plotHeight, y - height * plotHeight), col=boxBg, border=boxBorder, xpd=NA, ...)

	# legend title
	graphics::text(x + titleAdj[1] * legWidth, y - (1 - titleAdj[2]) * legHeight, labels=title, xpd=NA, ...)

	# color gradient
	colFx <- grDevices::colorRampPalette(col)
	cols <- colFx(99)

	# get gradient bounding box
	left <- x + gradAdjX[1] * legWidth
	right <- x + gradAdjX[2] * legWidth
	top <- y - (1 - gradAdjY[2]) * legHeight
	bottom <- y - (1 - gradAdjY[1]) * legHeight

	gradHeight <- top - bottom

	# plot (use many small rectangles)
	yInc <- seq(top, bottom, length.out=100)

	for (i in 1:99) graphics::polygon(c(left, right, right, left), c(yInc[i], yInc[i], yInc[i + 1], yInc[i + 1]), col=cols[i], border=NA, xpd=NA, ...)
	if (!is.na(border)) graphics::polygon(c(left, right, right, left), c(bottom, bottom, top, top), col=NA, border=border, xpd=NA, ...)

	# add labels
	if (!is.null(labels)) {

		labY <- seq(top, bottom, length.out=length(labels))
		text(x + legWidth * rep(labAdj, length(labels)), labY, labels=labels, pos=4, xpd=NA, ...)
	}

	# add swatches
	if (!is.null(swatches)) {

		for (i in seq_along(swatches)) {

			top <- y - (1 - swatches[[i]]$swatchAdjY[1]) * legHeight
			bottom <- y - (1 - swatches[[i]]$swatchAdjY[2]) * legHeight

			graphics::polygon(c(left, right, right, left), c(bottom, bottom, top, top), col=swatches[[i]]$col, border=swatches[[i]]$border, ...)
			labY <- top <- y - (1 - (mean(c(swatches[[i]]$swatchAdjY[[1]], swatches[[i]]$swatchAdjY[[2]])))) * legHeight
			text(x + legWidth * labAdj, labY, labels=swatches[[i]]$labels, pos=4, xpd=NA, ...)

		}

	}

}
