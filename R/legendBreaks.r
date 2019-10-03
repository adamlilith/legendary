#' Add a legend with thresholded values to a plot
#'
#' This function adds a legend to an existing plot that shows blocks of color. It is useful, for example, for displaying maps of continuous values that have been thresholded at multiple values. It first draws a "containing" box then inside the box a second box with a set of stacked bars, one per color. A legend title and labels for levels indicated by the color bar can be added.
#' @param x Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the top left of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'}, \code{'bottomright'}, \code{'top'}, \code{'bottom'}, \code{'left'}, \code{'right'}, or \code{'center'}).
#' @param y Numeric or \code{NULL}.
#' @param inset Numeric. If \code{x} is a word describing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertical offset.
#' @param horiz Logical, if \code{FALSE} (default), color boxes are plotted in along a column. If \code{TRUE}, then color boxes are plotted along a row.
#' @param width Numeric. Scaling factor for box width.
#' @param height Numeric. Scaling factor for box height.
#' @param labels Vector of characters of numeric values. Labels (from least to most) of levels of the focal variable indicated by the color ramp.
#' @param labAdjX Numeric or numeric vector of values typically between 0 and 1. If \code{horiz} is \code{FALSE} (default), then this a single value indicating the horizontal position of labels relative to the colored part of the legend. If \code{horiz} is \code{TRUE}, then this is a vector of numbers typically between 0 and 1 indicating horizontal position of \code{labels} relative to the colored part of the legend (0 = left, 1 = right).
#' @param labAdjY Numeric vector of values or a single numeric value typically between 0 and 1. If \code{horiz} is \code{FALSE} (default), then this a vector of numbers typically between 0 and 1 indicating the vertical position of \code{labels} relative to the colord part of the legend (0 = bottom, 1 = top). If \code{horiz} is \code{TRUE}, then this is a single value indicating the vertical position of labels relative to the containing box.
#' @param col List of characters or integers. Names of colors. The first color will be the lowest value and the last the highest value.
#' @param colBorder Characters or integer. Names of color to be used to draw an outline around each color in the color box. Use \code{NA} (default) to skip drawing an outline.
#' @param border Character or integer. Name (or integer code) of color to use to draw border of the color bar.
#' @param title Character or \code{NULL}. Name of title for the legend.
#' @param titleAdj Two numeric values between 0 and 1. Position of the legend relative to the legend box. The first pertains to horizontal positioning and the second vertical positioning.
#' @param adjX Two numeric values between 0 and 1. Size of the color bar in the x-dimension as a proportion of the legend box size. The first pertains to the left side of the bar and the second the right side.
#' @param adjY Two numeric values between 0 and 1. Size of the color bar in the y-dimension as a proportion of the legend box size. The first pertains to the bottom of the bar and the second the top.
#' @param boxBg Character or integer. Name (or integer code) of color to use to use for box containing legend. Leave as \code{NULL} to not draw a box.
#' @param boxBorder Character or integer. Name (or integer code) of color to use to use for box border containing legend. Leave as \code{NULL} to not draw a box border.
#' @param swatches A list or lists, each of which contains information on extra "swatches" of a single color to add above/below/on the color bar. These are useful, for example, for describing data that does not fall into the range covered by the data (e.g., \code{NA}'s).  If \code{swatches} is \code{NULL} then it is ignored.  Otherwise, it is a list of lists, and each sublist defines a different swatch. Sublists have these elements:
#' \itemize{
#' 	\item \code{swatchAdjY} Two numeric values typically between 0 and 1. This is only used if \code{horiz = FALSE} (default). Size of the swatches the in y-dimension as a proportion of the legend box size. The first pertains to the bottom side of the swatch and the second the top side.
#' 	\item \code{swatchAdjX} Two numeric values typically between 0 and 1. This is only used if \code{horiz = TRUE}. Size of the swatches the in x-dimension as a proportion of the legend box size. The first pertains to the left side of the swatch and the second the right side.
#' 	\item \code{col} Character or integer representing the swatch's color.
#' 	\item \code{border} Character or integer. Name (or integer code) of color to use to draw border of the swatch.
#' 	\item \code{labels} Character, for labeling the swatch.
#' }
#' @param ... Arguments to pass to \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}}, or \code{\link[graphics]{text}}.
#' @return Nothing (side effect is to add a legend to an existing graphics device).
#' @seealso \code{\link[graphics]{legend}}, \code{\link[legendary]{legendQuad}}
#' @examples
#' data(welfare)
#' 
#' # for visual clarity, put countries with high HIV on bottom
#' # of data frame so they're plotted last
#' welfare <- welfare[order(welfare$hivAge15to49_2018_perc), ]
#' welfare <- rbind(
#' 	welfare[is.na(welfare$hivAge15to49_2018_perc), ],
#' 	welfare[!is.na(welfare$hivAge15to49_2018_perc), ]
#' )
#' 
#' pop <- welfare$population2019
#' gdp <- welfare$gdp_2019usd
#' hiv <- welfare$hivAge15to49_2018_perc
#' hiv <- hiv / 100
#' 
#' gdpPerCap <- gdp / pop
#' 
#' # color categories
#' hivMax <- max(hiv, na.rm=TRUE)
#' cols <- rep(NA, length(hiv))
#' cols[hiv <= hivMax] <- 'darkred'
#' cols[hiv <= 0.75 * hivMax] <- 'red'
#' cols[hiv <= 0.5 * hivMax] <- 'lightsalmon'
#' cols[hiv <= 0.25 * hivMax] <- 'white'
#' cols[is.na(hiv)] <- 'gray'
#' 
#' # legend label positions
#' hivQuants <- hivMax * c(0, 0.25, 0.5, 0.75, 1)
#' hivLabelPos <- c(0, hivQuants / hivMax)
#' labels <- sprintf('%.2f', c(0, hivQuants))
#' 
#' # vertical alignment of legend
#' plot(log10(gdp), gdpPerCap,
#' 	pch=21,	cex=2, bg=cols, xlab='GDP (log 2019 USD)',
#'	ylab='2019 GDP per Capita (USD)',
#')
#'
#' legendBreaks(
#' 	x='bottomright',
#' 	y = NULL,
#' 	inset = 0.02,
#'	width = 0.23,
#'	height = 0.7,
#'	labels = labels,
#'	labAdjX = 0.75,
#'	labAdjY = hivLabelPos,
#'	col = c('white', 'lightsalmon', 'red', 'darkred'),
#'	border = 'black',
#'	title = 'HIV (%)',
#'	titleAdj = c(0.5, 0.9),
#'	adjX = c(0.2, 0.5),
#'	adjY = c(0.2, 0.8),
#'	boxBorder = 'black',
#'	swatches=list(
#'		list(
#'			swatchAdjY=c(0.05, 0.15),
#'			col='gray',
#'			border='black',
#'			labels='NA'
#'		)
#'	)
#')
#'
#'# horizontal alignment of legend
#'plot(log10(gdp), gdpPerCap,
#'	pch=21,	cex=2, bg=cols, xlab='GDP (log 2019 USD)',
#'	ylab='2019 GDP per Capita (USD)',
#')
#'
#'legendBreaks(
#'	x = 'topleft',
#'	y = NULL,
#'	horiz = TRUE,
#'	inset = 0.02,
#'	width = 0.7,
#'	height = 0.23,
#'	labels = labels,
#'	labAdjX = hivLabelPos,
#'	labAdjY = 0.13,
#'	col = c('white', 'lightsalmon', 'red', 'darkred'),
#'	border = 'black',
#'	title = 'HIV (%)',
#'	titleAdj = c(0.5, 0.9),
#'	adjX = c(0.2, 0.95),
#'	adjY = c(0.3, 0.8),
#'	boxBorder = NA,
#'	boxBg = NA,
#'	swatches=list(
#'		list(
#'			swatchAdjX=c(0.03, 0.18),
#'			col='gray',
#'			border='black',
#'			labels='NA'
#'		)
#'	)
#')
#' @export

legendBreaks <- function(
	x,
	y = NULL,
	inset = 0,
	horiz = FALSE,
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
	if (!is.null(labels) & !horiz) if (length(labels) != length(labAdjY)) stop('Length of argument "labels" must be same as length of "labAdjY".')
	if (!is.null(labels) & horiz) if (length(labels) != length(labAdjX)) stop('Length of argument "labels" must be same as length of "labAdjX".')

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

		x <- tolower(x)
	
		if (length(inset) == 1) inset <- c(inset, inset)

		xInset <- inset[1] * plotWidth
		yInset <- inset[2] * plotHeight

		legLeft <- legRight <- legTop <- legBottom <- NA
		
		if (x == 'topleft') {
		
			legLeft <- pos[1] + xInset
			legRight <- pos[1] + xInset + legWidth
			legTop <- pos[4] - yInset
			legBottom <- pos[4] - legHeight - yInset
		
		} else if (x == 'left') {
		
			legLeft <- pos[1] + xInset
			legRight <- pos[1] + xInset + legWidth
			legTop <- pos[3] + 0.5 * plotHeight + 0.5 * legHeight
			legBottom <- pos[3] + 0.5 * plotHeight - 0.5 * legHeight
		
		} else if (x == 'bottomleft') {
		
			legLeft <- pos[1] + xInset
			legRight <- pos[1] + xInset + legWidth
			legTop <- pos[3] + legHeight + yInset		
			legBottom <- pos[3] + yInset
		
		} else if (x == 'topright') {
		
			legLeft <- pos[2] - legWidth - xInset
			legRight <- pos[2] - xInset
			legTop <- pos[4] - yInset
			legBottom <- pos[4] - legHeight - yInset
		
		} else if (x == 'right') {
		
			legLeft <- pos[2] - legWidth - xInset
			legRight <- pos[2] - xInset
			legTop <- pos[3] + 0.5 * plotHeight + 0.5 * legHeight
			legBottom <- pos[3] + 0.5 * plotHeight - 0.5 * legHeight
		
		} else if (x == 'bottomright') {
		
			legLeft <- pos[2] - legWidth - xInset
			legRight <- pos[2] - xInset
			legTop <- pos[3] + legHeight + yInset		
			legBottom <- pos[3] + yInset
		
		} else if (x == 'center') {
		
			legLeft <- pos[1] + 0.5 * plotWidth - 0.5 * legWidth
			legRight <- pos[1] + 0.5 * plotWidth + 0.5 * legWidth
			legTop <- pos[3] + 0.5 * plotHeight + 0.5 * legHeight
			legBottom <- pos[3] + 0.5 * plotHeight - 0.5 * legHeight
		
		} else if (x == 'top') {
		
			legLeft <- pos[1] + 0.5 * plotWidth - 0.5 * legWidth
			legRight <- pos[1] + 0.5 * plotWidth + 0.5 * legWidth
			legTop <- pos[4] - yInset
			legBottom <- pos[4] - legHeight - yInset
		
		} else if (x == 'bottom') {
		
			legLeft <- pos[1] + 0.5 * plotWidth - 0.5 * legWidth
			legRight <- pos[1] + 0.5 * plotWidth + 0.5 * legWidth
			legTop <- pos[3] + legHeight + yInset		
			legBottom <- pos[3] + yInset
		
		} else {
			error('The "x" coordinate in function "legendGrad" must be a numeric value or\nan accepted position word (e.g., "top", "topleft", "bottomright", etc.).')
		}

	}

	# draw containing box
	if (!is.null(boxBorder)) graphics::polygon(c(legLeft, legRight, legRight, legLeft), c(legBottom, legBottom, legTop, legTop), col=boxBg, border=boxBorder, ...)

	# legend title
	xTitle <- legLeft + titleAdj[1] * legWidth
	yTitle <- legBottom + titleAdj[2] * legHeight
	graphics::text(xTitle, yTitle, labels=title, ...)

	# get color bounding box
	blockLeft <- legLeft + adjX[1] * legWidth
	blockRight <- legLeft + adjX[2] * legWidth
	blockTop <- legBottom + adjY[2] * legHeight
	blockBottom <- legBottom + adjY[1] * legHeight

	# get each patch's box dimensions
	blockHeight <- blockTop - blockBottom
	eachBlockHeight <- blockHeight / numCols

	blockWidth <- blockRight - blockLeft
	eachBlockWidth <- blockWidth / numCols

	# plot each patch
	for (countCol in seq_along(col)) {
	
		thisCol <- col[countCol]
		
		if (!horiz) {
		
			thisBottom <- blockBottom + eachBlockHeight * (countCol - 1)
			thisTop <- blockBottom + eachBlockHeight * countCol

			xs <- c(blockLeft, blockRight, blockRight, blockLeft)
			ys <- c(thisBottom, thisBottom, thisTop, thisTop)
			
		} else {
		
			thisLeft <- blockLeft + eachBlockWidth * (countCol - 1)
			thisRight <- blockLeft + eachBlockWidth * countCol

			xs <- c(thisLeft, thisRight, thisRight, thisLeft)
			ys <- c(blockBottom, blockBottom, blockTop, blockTop)
		
		}
		
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
	if (!is.na(border)) graphics::polygon(c(blockLeft, blockRight, blockRight, blockLeft), c(blockBottom, blockBottom, blockTop, blockTop), col=NA, border=border, ...)

	# add labels
	if (!is.null(labels)) {
		
		numLabs <- length(labels)
		if (!horiz) {

			labX <- legLeft + labAdjX * legWidth
			labY <- blockBottom + labAdjY * blockHeight
			labX <- rep(labX, numLabs)
			
		} else if (horiz) {

			labX <- blockLeft + labAdjX * blockWidth
			labY <- legBottom + labAdjY * legHeight
			labY <- rep(labY, numLabs)
			
		}

		text(labX, labY, labels=labels, ...)
		
	}
		
	# add swatches
	if (!is.null(swatches)) {

		for (i in seq_along(swatches)) {

			if (!horiz) {
			
				swatchTop <- legBottom + swatches[[i]]$swatchAdjY[2] * legHeight
				swatchBottom <- legBottom + swatches[[i]]$swatchAdjY[1] * legHeight
				
				swatchLeft <- blockLeft
				swatchRight <- blockRight

				labX <- legLeft + labAdjX * legWidth
				labY <- mean(c(swatchTop, swatchBottom))
				
			} else {
			
				swatchTop <- blockTop
				swatchBottom <- blockBottom
			
				swatchLeft <- legLeft + swatches[[i]]$swatchAdjX[1] * legWidth
				swatchRight <- legLeft + swatches[[i]]$swatchAdjX[2] * legWidth
			
				labX <- mean(c(swatchLeft, swatchRight))
				labY <- legBottom + labAdjY * legHeight

			}

			graphics::polygon(
				c(swatchLeft, swatchRight, swatchRight, swatchLeft),
				c(swatchBottom, swatchBottom, swatchTop, swatchTop),
				col=swatches[[i]]$col,
				border=swatches[[i]]$border,
				...
			)
			
			text(labX, labY, labels=swatches[[i]]$labels, ...)

		}

	}

}
