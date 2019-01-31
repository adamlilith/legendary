#' Adds a label to the upper left corner of a plot
#'
#' Adds a label to the upper left corner of a plot.
#' @param label Character.
#' @param adj A single number or two numbers. If a single number, then this is an adjustment factor that affects the position of the label in the y-direction relative to the default position. Positive connotes higher, negative lower. If this is two values, then the first adjusts in the x-direction (negative connotes left, positive right), and the second adjust in the y-direction.
#' @param ... Other arguments to send to \code{\link[graphics]{text}}.
#' @return Nothing (adds text to a plot).
#' @examples
#' par(mfrow=c(1, 2))
#' plot(1, 1)
#' labelFig('A: One')
#' plot(2, 2)
#' labelFig('B: Two')
#' @export

labelFig <- function(label, adj=0.05, ...) {

	if (length(adj) == 1L) adj <- c(0, adj)

	adjX <- adj[1]
	adjY <- adj[2]
	usr <- par('usr')

	y <- usr[4] + adjY * (usr[4] - usr[3])
	x <- usr[1] + adjX * (usr[2] - usr[1])
	
	text(x=x, y=y, labels=label, adj=1, xpd=NA, pos=4, ...)

}


