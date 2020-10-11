#' Locate a plot element (e.g., a legend)
#'
#' This is a private function that locates a plot element.
#' @param x Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the top left of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'}, \code{'bottomright'}, \code{'top'}, \code{'bottom'}, \code{'left'}, \code{'right'}, or \code{'center'}).
#' @param y Numeric or \code{NULL}.
#' @param inset Numeric. If \code{x} is a word describing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertical offset.
#' @param width Numeric. Scaling factor for box width.
#' @param height Numeric. Scaling factor for box height.
#' @param return Coordinates of the upper left of the element.
#' @keywords internal

.locateElement <- function(
	x,
	y = NULL,
	inset = 0,
	width = 0.2,
	height = 0.5
) {

	# get coordinate stats for existing plot
	position <- par('usr')

	plotWidth <- position[2] - position[1]
	plotHeight <- position[4] - position[3]

	legWidth <- width * plotWidth
	legHeight <- height * plotHeight

	# get containing box location top left coordinate
	if (class(x) == 'character') {

		if (length(inset) == 1) inset <- c(inset, inset)

		xInset <- inset[1] * plotWidth
		yInset <- inset[2] * plotHeight

		if (x == 'topleft') {
			x <- position[1] + xInset
			y <- position[4] - yInset
		} else if (x == 'topright') {
			x <- position[2] - legWidth - xInset
			y <- position[4] - yInset
		} else if (x == 'bottomleft') {
			x <- position[1] + xInset
			y <- position[3] + legHeight + yInset
		} else if (x == 'bottomright') {
			x <- position[2] - legWidth - xInset
			y <- position[3] + legHeight + yInset
		} else if (x == 'bottom') {
			x <- position[1] + 0.5 * plotWidth - 0.5 * legWidth
			y <- position[3] + legHeight + yInset
		} else if (x == 'top') {
			x <- position[1] + 0.5 * plotWidth - 0.5 * legWidth
			y <- position[4] - yInset
		} else if (x == 'left') {
			x <- position[1] + xInset
			y <- position[3] + 0.5 * plotHeight + 0.5 * legHeight
		} else if (x == 'right') {
			x <- position[2] - xInset - legWidth
			y <- position[3] + 0.5 * plotHeight + 0.5 * legHeight
		} else if (x == 'center') {
			x <- position[1] + 0.5 * (position[2] - position[1]) - 0.5 * legWidth
			y <- position[3] + 0.5 * plotHeight + 0.5 * legHeight
		} else {
			stop('The "x" coordinate in function "legendGrad" must be a numeric value or\nan accepted position word (e.g., "top", "topleft", "bottomright", etc.).')
		}

	}

	list(xy=c(x, y), plotHeight=plotHeight, plotWidth=plotWidth, legHeight=legHeight, legWidth=legWidth)

}
