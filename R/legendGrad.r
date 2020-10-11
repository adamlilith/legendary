#' Add a gradient legend to a plot
#'
#' This function adds a legend to an existing plot that shows a gradient in color. It first draws a "containing" box then a bar with a color gradient inside the box. A legend title and labels for levels indicated by the color bar can be added.
#' @param x Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the top left of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'}, \code{'bottomright'}, \code{'top'}, \code{'bottom'}, \code{'left'}, \code{'right'}, or \code{'center'}).
#' @param y Numeric or \code{NULL}.
#' @param inset Numeric. If \code{x} is a word describing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertical offset.
#' @param vert Logical, if \code{TRUE} (default), then the gradient will be drawn vertically and labels plotted to the left or right of the gradient. If \code{FALSE}, then the gradient will be drawn horizontally and labels will be plotted above or below the gradient. Note that if \code{vert} is \code{FALSE} then many of the default values will likely need to be changed.
#' @param width Numeric. Scaling factor for box width.
#' @param height Numeric. Scaling factor for box height.
#' @param labels Vector of characters of numeric values. Labels (from least to most) of levels of the focal variable indicated by the color ramp.
#' @param labAdj Numeric between 0 and 1. If \code{vert} is \code{TRUE} then this is the horizontal position of labels relative to the containing box. If \code{vert} is \code{FALSE} then this is the vertical position relative to the containing box.
#' @param labPos 1 (right-align labels), 2 (bottom-align labels), 3 (left-align labels), or 4 (top-align labels).
#' @param labCex Positive numeric, size of label text.
#' @param col List of characters or integers. Names of colors to be used to create a gradient to fill the legend bar. The first color will be the lowest value and the last the highest value.
#' @param border Character or integer. Name (or integer code) of color to use to draw border of the gradient bar.
#' @param title Character or \code{NULL}. Name of title for the legend.
#' @param titleAdj Two numeric values between 0 and 1. Position of the legend title relative to the container box. The first pertains to horizontal positioning and the second vertical positioning.
#' @param titlePos 1 (right-align title), 2 (bottom-align title), 3 (left-align title), or 4 (top-align title).
#' @param titleCex Positive numeric, size of title text.
#' @param adjX Two numeric values between 0 and 1. Size of the gradient bar in the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.
#' @param adjY Two numeric values between 0 and 1. Size of the gradient bar in the y-dimension as a proportion of the container box size. The first pertains to the bottom of the bar and the second the top.
#' @param boxBg Character or integer. Name (or integer code) of color to use to use for box containing legend. Leave as \code{NULL} to not draw a box.
#' @param boxBorder Character or integer. Name (or integer code) of color to use to use for box border containing legend. Leave as \code{NULL} to not draw a box border.
#' @param swatches A list or lists, each of which contains information on extra "swatches" of a single color to add above/below/on the gradient bar. These are useful, for example, for describing data that does not fall into the range covered by the data (e.g., \code{NA}'s).  If \code{swatches} is \code{NULL} then it is ignored.  Otherwise, it is a list of lists, and each sublist defines a different swatch. Sublists have these elements:
#' \itemize{
#' 	\item \code{swatchAdjY} Two numeric values between 0 and 1. Size of the swatches the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.  (Swatches will always be left-right aligned with the gradient bar.)
#' 	\item \code{col} Character or integer representing the swatch's color.
#' 	\item \code{labels} Character, for labelling the swatch.
#' 	\item \code{...} Other values to pass to \code{\link[graphics]{polygon}} and \code{\link[graphics]{text}} for formatting the swatch and its label (e.g., \code{border}, \code{lwd}, \code{lty}, \code{pos}, \code{cex}, etc.). These values will override any specified in \code{...}.
#' }
#' @param ... Arguments to pass to \code{\link{plot}}, \code{\link[graphics]{polygon}}, or \code{\link[graphics]{text}}.
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
#' 	labAdjX = 0.4,
#' 	col = c('white', 'red'),
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
#' 			labels='NA',
#'			cex=1.2,
#' 			border='black',
#'			lwd=2,
#'			lty='dotted'
#' 		)
#' 	)
#' )
#' @export

legendGrad <- function(
	x,
	y = NULL,
	inset = 0,
	vert = TRUE,
	width = 0.2,
	height = 0.5,
	labels = c(0, 0.33, 0.67, 1),
	labAdj = 0.75,
	labPos = 4,
	labCex = 1,
	col = c('yellow', 'orange', 'red'),
	border = 'black',
	title = 'Title',
	titleAdj = c(0.5, 0.9),
	titlePos = NULL,
	titleCex = 1,
	adjX = c(0.2, 0.65),
	adjY = c(0.1, 0.8),
	boxBg = par('bg'),
	boxBorder = 'black',
	swatches = NULL,
	...
) {

	dots <- list(...)

	location <- .locateElement(x=x, y=y, inset=inset, width=width, height=height)
	x <- location$xy[1]
	y <- location$xy[2]
	plotHeight <- location$plotHeight
	plotWidth <- location$plotWidth
	legHeight <- location$legHeight
	legWidth <- location$legWidth

	# draw containing box
	if (!is.null(boxBorder)) graphics::polygon(c(x, x + width * plotWidth, x + width * plotWidth, x), c(y, y, y - height * plotHeight, y - height * plotHeight), col=boxBg, border=boxBorder, xpd=NA, ...)

	# legend title
	graphics::text(x + titleAdj[1] * legWidth, y - (1 - titleAdj[2]) * legHeight, labels=title, pos=titlePos, cex=titleCex, xpd=NA, ...)

	# color gradient
	colFx <- grDevices::colorRampPalette(col)
	cols <- colFx(99)

	# get gradient bounding box
	left <- x + adjX[1] * legWidth
	right <- x + adjX[2] * legWidth
	top <- y - (1 - adjY[2]) * legHeight
	bottom <- y - (1 - adjY[1]) * legHeight

	gradHeight <- top - bottom

	# plot (use many small rectangles)
	if (vert) {
		
		inc <- seq(bottom, top, length.out=100)

		for (i in 1:99) graphics::polygon(c(left, right, right, left), c(inc[i], inc[i], inc[i + 1], inc[i + 1]), col=cols[i], border=NA, xpd=NA, ...)
		
	} else {
	
		inc <- seq(left, right, length.out=100)
		for (i in 1:99) graphics::polygon(c(inc[i], inc[i + 1], inc[i + 1], inc[i]), c(bottom, bottom, top, top), col=cols[i], border=NA, xpd=NA, ...)
		
	}
		
	if (!is.na(border)) graphics::polygon(c(left, right, right, left), c(bottom, bottom, top, top), col=NA, border=border, xpd=NA, ...)

	# add labels
	if (!is.null(labels)) {

		if (vert) {
			labY <- seq(bottom, top, length.out=length(labels))
			text(x + legWidth * rep(labAdj, length(labels)), labY, labels=labels, pos=labPos, cex=labCex, xpd=NA, ...)
		} else {
			labX <- seq(left, right, length.out=length(labels))
			text(labX, y + legHeight * rep(labAdj, length(labels)), labels=labels, pos=labPos, cex=labCex, xpd=NA, ...)
		}
		
	}

	# add swatches
	if (!is.null(swatches)) {

		for (i in seq_along(swatches)) {

			top <- y - (1 - swatches[[i]]$swatchAdjY[1]) * legHeight
			bottom <- y - (1 - swatches[[i]]$swatchAdjY[2]) * legHeight

			swatchPos <- if (any(names(swatches[[i]]) == 'pos')) {
				swatches[[i]]$pos
			} else if (any(names(dots) == 'pos')) {
				dots$pos
			} else {
				NULL
			}

			swatchCex <- if (any(names(swatches[[i]]) == 'cex')) {
				swatches[[i]]$cex
			} else if (any(names(dots) == 'cex')) {
				dots$cex
			} else {
				par('cex')
			}

			swatchLwd <- if (any(names(swatches[[i]]) == 'lwd')) {
				swatches[[i]]$lwd
			} else if (any(names(dots) == 'lwd')) {
				dots$lwd
			} else {
				par('lwd')
			}

			swatchLty <- if (any(names(swatches[[i]]) == 'lty')) {
				swatches[[i]]$lty
			} else if (any(names(dots) == 'lty')) {
				dots$lty
			} else {
				par('lty')
			}

			swatchBorder <- if (any(names(swatches[[i]]) == 'border')) {
				swatches[[i]]$border
			} else if (any(names(dots) == 'border')) {
				dots$border
			} else {
				par('fg')
			}

			graphics::polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col=swatches[[i]]$col, xpd=NA, lwd=swatchLwd, lty=swatchLty, border=swatchBorder, ...)
			
			labY <- top <- y - (1 - (mean(c(swatches[[i]]$swatchAdjY[[1]], swatches[[i]]$swatchAdjY[[2]])))) * legHeight
			text(x + legWidth * labAdj, labY, labels=swatches[[i]]$labels, pos=swatchPos, cex=swatchCex, xpd=NA, ...)

		}

	}

}
