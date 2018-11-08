#' Creates a "four-color" plot for interpreting data with mixtures of four aspects.
#'
#' This function creates a "quad-plot" used for interpreting data that represents four aspects of each subject (e.g., the probability that an individual belongs to one of four populations). The plot is typically a square with each corner assigned a different color. The closer an object in the main plot is to one of four "assignments" (e.g., populations), the more the corresponding color is shown, with assignments of intermediate value displaying as a mixture of colors. Labels for the four corners can be added.
#' @param x Data frame or matrix with 4 columns, each representing the relative probabilities of belonging to one of 4 aspects. Each row will be represented by a point on the plot.
#' @param cols Character list. Names of four colors to be used to create a gradient to fill the color swatch. The first color will be assigned to the lower left corner, the second to the upper left, third to upper right, and fourth to lower right. Note that not every color combination produces a map with a unique color at each coordinate of the color swatch.
#' @param labels Character vector used to name the four corners of the color swatch in this order: bottom left, top left, top right, bottom right. Leave as \code{NULL} to skip displaying labels. The default is to use the names of the four columns of \code{x}.
#' @param labelCex Numeric, size of label text.
#' @param labelAdj Numeric, usually between 0 and 1. Position of corner labels relative to the corners of the swatch.
#' @param background Logical, if \code{TRUE} then color the background of the plot using the color-coding scheme.
#' @param n Positive integer, number of squares used to approximate a smooth color map for the background. This is the number of cells along each side of the swatch. Higher values create less "blocky" plots but increase drawing time.
#' @param style Character, either \code{'pies'} in which case each point is represented by a \code{link[omnibusPlots]{pies}} plot or \code{'points'} in which case each point is represented by a point with its color a product of its four aspects.
#' @param pch Used when \code{style} is \code{'points'}. Either a number or \code{.} indicating the type of symbol to draw (see \code{link[graphics]{points}}).
#' @param na.rm Logical, if \code{TRUE} then remove lines in \code{x} that have at least one \code{NA}.
#' @param ... Arguments to pass to \code{\link[graphics]{points}}, \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}}, or \code{\link[graphics]{text}}.
#' @return Nothing (side effect is to create a plot).
#' @details Note that there is not a unique (1-to-1) mapping of the 4 values to position on the plot. For example, if belonging solely to population A is represented by placing a point in the lower left corner, population B in the upper left, C the upper right, and D the lower right, then belonging strongly to population A and weakly to C will place a point near the location close to the location where a point belonging strongly to A and weakly to B and D will also lie.
#' @seealso \code{\link[legendary]{colorFrom4Vector}}, \code{\link[legendary]{legendQuad}} 
#' @examples
#' data(religion)
#' head(religion)
#' religion$others <- rowSums(religion[ , c('unaffiliated', 'hindu', 'folk', 'other', 'jewish')])
#' religs <- c('christian', 'muslim', 'buddhist', 'others')
#' par(mfrow=c(1, 2))
#' plotQuad(religion[ , religs], main='Religion by Country', radius=0.04)
#' plotQuad(religion[ , religs], 'points', background=FALSE, main='Religion by Country', cex=2)
#' @export
plotQuad <- function(
	x,
	cols = c('white', 'cyan', 'black', 'red'),
	main = '',
	labels = colnames(x),
	labelCex = 0.6,
	labelAdj = 1,
	background = TRUE,
	n = 25,
	style = 'pies',
	pch = 21,
	radius = 0.01,
	border = 'black',
	na.rm = FALSE,
	...
) {

	dots <- as.list(...)

	# plotArgs <- c(names(formals(plot.default)), names(par()))
	# axisArgs <- c(names(formals(axis)))
	# piesArgs <- c(names(formals(omnibusPlots::pies)))
	# pointsArgs <- c(names(formals(graphics::points)))
	# labelsArgs <- c(names(formals(graphics::text)))

	# plotArgs
	
	# leg.args.unique <- c("legend", "fill", "border", "angle", "density", "box.lwd", "box.lty", "box.col", "pt.bg", "pt.cex", "pt.lwd", "xjust", "yjust", "x.intersp", "y.intersp", "text.width", "text.col", "merge", "trace", "plot", "ncol", "horiz", "title", "inset", "title.col", "title.adj")
    # leg.args.all <- c(leg.args.unique, "col", "lty", "lwd", "pch", "bty", "bg", "cex", "adj", "xpd")
    # dots <- list(...)
    # do.call('plot', c(list(x = x, y = x), dots))
    # do.call('legend', c(list("bottomleft", "bar"), dots[names(dots) %in% leg.args.all]))

	par(pty='s', bty='n')
	dotsForPlot <- dots[!(c('pch', 'xlab', 'ylab', 'xaxt', 'yaxt', 'x', 'y') %in% names(dots))]
	do.call('plot', args=list(c(x=NA, y=NA, xaxt='n', yaxt='n', xlab='', ylab='', unlist(dotsForPlot))))
	print('a')
	# plot(x=NA, y=NA, xlim=c(0, 1), ylim=c(0, 1), xaxt='n', yaxt='n', xlab='', ylab='', unlist(dots))
	axis(1, labels=FALSE, tick=FALSE)
	axis(2, labels=FALSE, tick=FALSE)

	### background
	if (background) {
	
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

		# plot background
		for (i in 1:nrow(pixels)) {
			
			xPixel <- pixels[i, 'x']
			yPixel <- pixels[i, 'y']
			
			xSides <- c(xPixel - by / 2, xPixel + by / 2, xPixel + by / 2, xPixel - by / 2)
			xSides <- pmax(0, xSides)
			xSides <- pmin(1, xSides)
			
			ySides <- c(yPixel - by / 2, yPixel - by / 2, yPixel + by / 2, yPixel + by / 2)
			ySides <- pmin(1, ySides)
			ySides <- pmax(0, ySides)

			polygon(xSides, ySides, col=hsv(pixels[i, 'h'], pixels[i, 's'], pixels[i, 'v']), border=NA)
			
		}
		
	}
	
	if (na.rm && naRows(x) > 0) x <- x[-naRows(x), ]
	
	for (i in 1:nrow(x)) {
	
		props <- c(x[i, 1], x[i, 2], x[i, 3], x[i, 4])
		this <- colorFrom4Vector(props, cols = cols)
		if (style == 'pies') {
		
			pies(props, add=TRUE, xPos=this$xy[['x']], yPos=this$xy[['y']], col=cols, xpd=NA, ...)
		
		} else if (style == 'points') {
		
			points(this$xy[['x']], this$xy[['y']], xpd=NA, bg=hsv(h=this$col[['h']], s=this$col[['s']], v=this$col[['v']]), ...)
		
		} else {
			stop('Argument "style" in function "plotQuad()" must be either "pies" or "points".')
		}
	
	}
		
	# labels
	text(0, 0, labels=labels[1], adj=c(0.5, labelAdj), xpd=NA, cex=labelCex, ...)
	text(0, 1, labels=labels[2], adj=c(0.5, -0.5 * labelAdj), xpd=NA, cex=labelCex, ...)
	text(1, 1, labels=labels[3], adj=c(0.5, -0.5 * labelAdj), xpd=NA, cex=labelCex, ...)
	text(1, 0, labels=labels[4], adj=c(0.5, labelAdj), xpd=NA, cex=labelCex, ...)
	
}
