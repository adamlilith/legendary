#' Plots two horizontal barplots back-to-back.
#'
#' This function plots two horizontal barplots back-to-back.
#' @param x1 Numeric, vector of values for the left-hand side of the plot. Values are plotted such that the first is represented by the lowest bar, the second the next-higher bar, and so on.
#' @param x2 Numeric, vector of values for the right-hand side of the plot, must be same length as \code{x1}.
#' @param sameScale Logical, if \code{TRUE} (default) then use the same scale for both sides of the plot. If \code{FALSE}, then each side will have its own scale determined automatically \emph{unless} \code{xlim1} and/or \code{xlim2} are non-\code{NULL}.
#' @param xlim1,xlim2 Two numeric values defining the x-axis limits for \code{x1} and \code{x2}. Alternatively, if one or both are \code{NULL} (default), then the scale is determined automatically for either or both.
#' @param tickLabelOffset Numeric, offset (in y-direction) of labels for x-axis tick marks.
#' @param xlab1,xlab2 Characters, names of each x-axis label. Note that by default these are used but if \code{xlab} is non-\code{NULL}, then it is used insetad.
#' @param xlab Character, name of x-axis label.
#' @param xlabOffset Numeric, offset (in y-direction) of x-axis label(s).
#' @param tickSize Numeric, relative size of tick marks. Positive numbers grow them longer from the bottom of the x-axis, negative upward.
#' @param labels Vector of same length as \code{x1} or \code{x2} with labels for bars. Can be numeric, character, integers, or anything else that prints. Default is \code{NULL}, in which case no labels are added.
#' @param labelSide Character, either \code{left}, \code{right}, or \code{both}, indicating side on which bar labels are drawn.
#' @param labelOffset Numeric, offset (from border or plot region) for positioning of labels. Positive numbers move labels outward, negative inward.
#' @param cexLab Either \code{NA} (default) or a numeric > 0. Size of label text.
#' @param col1,col2 Colors of bars.
#' @param border1,border2 Border colors of bars.
#' @param ... Arguments to pass to \code{\link[graphics]{par}} or \code{\link[graphics]{plot}}.
#' @details
#' @return None. Side-effect is to generate a plot.
#' @seealso \code{\link{barplot}}
#' @examples
#' x1 <- 10:1
#' x2 <- sample(11:20, 10)
#' labels <- LETTERS[10:1]
#' pancakes(x1, x2, labels = labels)
#' pancakes(x1, x2, sameScale = FALSE, labels=labels)
#' @export

pancakes <- function(
	x1, x2,
	sameScale = TRUE,
	xlim1 = NULL,
	xlim2 = NULL,
	tickLabelOffset = -0.05,
	xlab1 = 'x1',
	xlab2 = 'x2',
	xlab = NULL,
	xlabOffset = -0.12,
	tickSize = 0.01,
	labels = NULL,
	labelSide = 'both',
	cexLab = 1,
	col1 = 'gray70',
	col2 = 'gray30',
	border1 = 'black',
	border2 = 'black',
	...
) {

	if (length(x1) != length(x2)) stop('Arguments "x1" and "x2" must be of same length.')
	if (any(na.omit(x1) < 0)) warning('Argument "x1" contains negative values.')
	if (any(na.omit(x2) < 0)) warning('Argument "x2" contains negative values.')

	# create x-axis limits
	# actual plot will have natural range [-1, 1] along x-axis
	# barplots will meet at x = 0
	# y-axis will have natural range [0, 1]
	
	# establish plot
	plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(-1, 1), ylim=c(0, 1), ...)
	lines(x=c(-1, 1), y=c(-0.01, -0.01), xpd=NA, ...)
	
	# establish x-axes' limits
	if (sameScale) {
		if (is.null(xlim1) & is.null(xlim2)) {
			xlim1 <- xlim2 <- pretty(c(x1, x2))
		} else if (is.null(x1)) {
			xlim1 <- pretty(x1)
			xlim2 <- pretty(c(x2, xlim2))
		} else if (is.null(x2)) {
			xlim1 <- pretty(c(x1, xlim1))
			xlim2 <- pretty(x2)
		}
	} else {
		if (is.null(xlim1) & is.null(xlim2)) {
			xlim1 <- pretty(x1)
			xlim2 <- pretty(x2)
		} else if (is.null(x1)) {
			xlim1 <- pretty(x1)
			xlim2 <- pretty(c(x2, xlim2))
		} else if (is.null(x2)) {
			xlim1 <- pretty(c(x1, xlim1))
			xlim2 <- pretty(x2)
		}
	}
	
	# create mapping function from x-axis's intended and natural scale
	x1scaler <- function(x, low=xlim1[1], high=tail(xlim1, 1)) -(x - low) / (high - low)
	x2scaler <- function(x, low=xlim2[1], high=tail(xlim2, 1)) (x - low) / (high - low)
	
	# bar width
	width <- 1 / length(x1)
	halfWidth <- w / 2
	
	# left side bars
	sizes <- x1scaler(x1)
	for (i in seq_along(x1)) {
	
		y <- i * 2 * halfWidth
	
		polygon(
			x=c(0, 0, sizes[i], sizes[i]),
			y=c(y - halfWidth, y + halfWidth, y + halfWidth, y - halfWidth) - halfWidth,
			col=col1, border=border1, xpd=NA, ...
		)
		
	}

	# right side bars
	sizes <- x2scaler(x2)
	for (i in seq_along(x2)) {
		
		y <- i * 2 * halfWidth
	
		polygon(
			x=c(0, 0, sizes[i], sizes[i]),
			y=c(y - halfWidth, y + halfWidth, y + halfWidth, y - halfWidth) - halfWidth,
			col=col2, border=border2, xpd=NA, ...
		)
		
	}
	
	# add labels
	if (!is.null(labels)) {
		y <- seq_along(x1) / length(x1) - halfWidth
		if (labelSide == 'left') {
			text(x=-1 - labelOffset, y=y, labels=labels, pos=2, xpd=NA, cex=cexLab, ...)
		} else if (labelSide == 'right') {
			text(x=1 + labelOffset, y=y, labels=labels, pos=4, xpd=NA, cex=cexLab, ...)
		} else if (labelSide == 'both') {
			text(x=-1 - labelOffset, y=y, labels=labels, pos=2, xpd=NA, cex=cexLab, ...)
			text(x=1 + labelOffset, y=y, labels=labels, pos=4, xpd=NA, cex=cexLab, ...)
		}
	}

	# add tick marks and values along left x-axis
	ats <- x1scaler(xlim1)
	for (i in seq_along(ats)) lines(x=c(ats[i], ats[i]), y=c(-0.01, -0.01 - tickSize), xpd=NA)
	labs <- c('', xlim1[2:length(xlim1)])
	text(x=ats, y=tickLabelOffset, labels=labs, xpd=NA, ...)
	
	# add tick marks and values along right x-axis
	ats <- x2scaler(xlim2)
	for (i in seq_along(ats)) lines(x=c(ats[i], ats[i]), y=c(-0.01, -0.01 - tickSize), xpd=NA)
	labs <- c('', xlim2[2:length(xlim2)])
	text(x=ats, y=tickLabelOffset, labels=labs, xpd=NA, ...)
	
	if (xlim1[1] == xlim2[1]) text(x=0, y=-0.05 * cexLab, labels=xlim1[1], xpd=NA, ...)
	
	# x-axis label
	if (is.null(xlab)) {
		text(x1scaler(mean(xlim1)), xlabOffset, labels=xlab1, xpd=NA, ...)
		text(x2scaler(mean(xlim2)), xlabOffset, labels=xlab2, xpd=NA, ...)
	} else {
		text(0, -0.12 * cexLab, labels=xlab, xpd=NA, ...)
	}

}
