#' Creates a "four-color" plot legend for interpreting data with mixtures of four aspects.
#'
#' This function adds a legend to an existing plot. The legend is typically a square with each corner assigned a different color. The closer an object in the main plot is to one of four "assignments" (e.g., populations), the more the corresponding color is shown, with assignments of intermediate value displaying as a mixture of colors. The function first draws a "containing" box then a square "color swatch" with a color gradient inside. A legend title and labels for the four corners can be added.
#' @param x Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the top left of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'}, \code{'bottomright'}, \code{'top'}, \code{'bottom'}, \code{'left'}, or \code{'right'}).
#' @param y Numeric or \code{NULL}. Y-coordinate of the top left corner of the legend.
#' @param inset Numeric. If \code{x} is a word describing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertical offset.
#' @param width Numeric. Scaling factor for box width.
#' @param height Numeric. Scaling factor for box height.
#' @param labels Character vector used to name the four corners of the color swatch in this order: bottom left, top left, top right, bottom right. Leave as \code{NULL} to skip displaying labels.
#' @param labelAdj Numeric, usually between 0 and 1. Position of corner labels relative to the corners of the swatch.
#' @param cols Character list. Names of four colors to be used to create a gradient to fill the color swatch. The first color will be assigned to the lower left corner, the second to the upper left, third to upper right, and fourth to lower right. Note that not every color combination produces a map with a unique color at each coordinate of the color swatch.
#' @param n Positive integer, number of squares used to approximate a smooth color map in the swatch. This is the number of cells along each side of the swatch. Higher values create less "blocky" swatches but increase drawing time.
#' @param border Character or integer. Name (or integer code) of color to use to draw border of the color swatch.  Leave as \code{NULL} to skip drawing a border.
#' @param title Character or \code{NULL}. Name of title for the legend.
#' @param titleAdj Two numeric values between 0 and 1. Position of the legend relative to the container box. The first pertains to horizontal positioning and the second vertical positioning.
#' @param swatchAdjX Two numeric values between 0 and 1. Size of the color swatch in the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.
#' @param swatchAdjY Two numeric values between 0 and 1. Size of the swatch in the y-dimension as a proportion of the container box size. The first pertains to the top of the bar and the second the bottom. Alternatively, if \code{aspect} is \code{TRUE} then the first value of \code{swatchAdjY} is used to place the top of the swatch but the bottom is located such that the swatch is square (i.e., the second value of \code{swatchAdjY} is ignored).
#' @param aspect Logical, if \code{TRUE} then the height of the color swatch is scaled by \code{swatchAdjX} so that the swatch is square (and \code{swatchAdjY} is ignored).  If \code{FALSE} then the height is determined by \code{swatchAdjY}.
#' @param boxBg Character or integer. Name (or integer code) of color to use to use for box containing legend. Leave as \code{NULL} to not draw a box.
#' @param boxBorder Character or integer. Name (or integer code) of color to use to use for box border containing legend. Leave as \code{NULL} to not draw a box border.
#' @param ... Arguments to pass to \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}}, or \code{\link[graphics]{text}}.
#' @return Nothing (side effect is to add a legend to an existing graphics device).
#' @seealso \code{\link[graphics]{legend}}, \code{\link[graphics]{gradLegend}},
#' @examples
#' data(religion)
#' head(religion)
#' religion$others <- rowSums(religion[ , c('unaffiliated', 'hindu', 'folk', 'other', 'jewish')])
#' religs <- c('christian', 'muslim', 'buddhist', 'others')
#' par(mfrow=c(1, 2))
#' plotQuad(religion[ , religs], 'points', background=FALSE, main='Religion by Country')
#' legendQuad('top', inset=0.01, labels=colnames(relig))
#' @export

legendQuad <- function(
	x,
	y = NULL,
	inset = 0,
	width = 0.2,
	height = 0.2,
	labels = LETTERS[1:4],
	labelAdj = 1,
	cols = c('white', 'cyan', 'black', 'red'),
	n = 25,
	border = par('fg'),
	title = '',
	titleAdj = c(0.5, 0.9),
	aspect = FALSE,
	swatchAdjX = c(0.15, 0.85),
	swatchAdjY = c(0.25, 0.85),
	boxBg = par('bg'),
	boxBorder = par('fg'),
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
			x <- 0.5 * plotWidth
			y <- pos[4] + 0.1 * plotHeight
		} else if (x == 'top') {
			x <- 0.5 * plotWidth
			y <- pos[3] - 0.1 * plotHeight
		} else if (x == 'left') {
			x <- pos[1] + 0.1 * plotWidth
			y <- 0.5 * plotHeight
		} else if (x == 'right') {
			x <- pos[2] - 0.1 * plotWidth
			y <- 0.5 * plotHeight
		} else {
			error('The "x" coordinate must be a numeric value or an accepted position word (e.g., "top", "topleft", "bottomright", etc.).')
		}

	}

	# legend box
	graphics::polygon(c(x, x + width * plotWidth, x + width * plotWidth, x), c(y, y, y - height * plotHeight, y - height * plotHeight), col=boxBg, border=boxBorder, ...)

	# get swatch bounding box
	swatchLeft <- x + swatchAdjX[1] * legWidth
	swatchRight <- x + swatchAdjX[2] * legWidth

	swatchWidth <- swatchRight - swatchLeft

	swatchTop <- y - swatchAdjY[1] * legHeight
	if (aspect) {
		asp <- plotHeight / plotWidth
		swatchBottom <- swatchTop - asp * swatchWidth
	} else {
		swatchBottom <- y - swatchAdjY[2] * legHeight
	}


	swatchHeight <- swatchTop - swatchBottom

	### swatch
	pixels <- matrix(NA, ncol=10, nrow=1)
	colnames(pixels) <- c('x1', 'x2', 'x3', 'x4', 'sum', 'x', 'y', 'h', 's', 'v')

	by <- 1 / (n - 1)

	# swatch pixel stats
	for (x1 in seq(0, 1, by = by)) {
		for (x2 in seq(0, 1 - x1, by = by)) {
			for (x3 in seq(0, 1 - x1 - x2, by = by)) {

				x4 <- 1 - x1 - x2 - x3
				props <- c(x1, x2, x3, x4)

				col <- colorFrom4Vector(props, cols=cols)

				thisPixel <- matrix(c(x1, x2, x3, x4, sum(props), col$xy, col$col), nrow=1)
				pixels <- rbind(pixels, thisPixel)

			}

		}

	}

	pixels <- pixels[-1, ]

	pixels <- stats::aggregate(pixels, by=list(pixels[ , 'x'], pixels[ , 'y']), mean)
	xyCols <- which(colnames(pixels) %in% c('x', 'y'))
	keep <- 1:ncol(pixels)
	keep <- keep[!(keep %in% xyCols)]
	pixels <- pixels[ , keep]
	colnames(pixels)[1:2] <- c('x', 'y')

	# plot swatch
	for (i in 1:nrow(pixels)) {

		xPixelUnscaled <- pixels[i, 'x']
		yPixelUnscaled <- pixels[i, 'y']

		xPixel <- swatchLeft + xPixelUnscaled * swatchWidth
		yPixel <- swatchBottom + yPixelUnscaled * swatchHeight

		pixelWidth <- by * swatchWidth
		pixelHeight <- by * swatchHeight

		xSides <- c(xPixel - pixelWidth / 2, xPixel + pixelWidth / 2, xPixel + pixelWidth / 2, xPixel - pixelWidth / 2)
		xSides <- pmax(swatchLeft, xSides)
		xSides <- pmin(swatchRight, xSides)

		ySides <- c(yPixel - pixelHeight / 2, yPixel - pixelHeight / 2, yPixel + pixelHeight / 2, yPixel + pixelHeight / 2)
		ySides <- pmin(swatchTop, ySides)
		ySides <- pmax(swatchBottom, ySides)

		polygon(xSides, ySides, col=hsv(pixels[i, 'h'], pixels[i, 's'], pixels[i, 'v']), border=NA)

	}

	if (!is.na(border)) graphics::polygon(c(swatchLeft, swatchRight, swatchRight, swatchLeft), c(swatchBottom, swatchBottom, swatchTop, swatchTop), col=NA, border=border, ...)

	# labels
	text(swatchLeft, swatchBottom, labels=labels[1], adj=c(0.5, labelAdj), ...)
	text(swatchLeft, swatchTop, labels=labels[2], adj=c(0.5, -0.5 * labelAdj), ...)
	text(swatchRight, swatchTop, labels=labels[3], adj=c(0.5, -0.5 * labelAdj), ...)
	text(swatchRight, swatchBottom, labels=labels[4], adj=c(0.5, labelAdj), ...)

	# legend title
	graphics::text(x + titleAdj[1] * legWidth, y - (1 - titleAdj[2]) * legHeight, labels=title, ...)

}
