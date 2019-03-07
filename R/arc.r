#' Plot an arc on an existing plot
#'
#' This function adds an arc to an existing plot.
#' @param x Numeric, x-coordinate of center of arc.
#' @param y Numeric, y-coordinate of center of arc.
#' @param radius Numeric, radius of arc.
#' @param deg Two-element numeric vector, degrees across which to draw arc (0 is up, 90 right, 180 down, and 270 left).
#  @param n Integer > 1, number of segments used to draw arc.
#' @param ... Arguments to send to \code{\link[graphics]{lines}}.
#' @return Nothing (side-effect is to add an arc to an existing plot).
#' @seealso \code{\link[graphics]{lines}}
#' @examples
#' par(pty='s')
#' plot(0, 0, xlim=c(-1, 1), ylim=c(-1, 1))
#' arc(x=0.3, y=-0.2, radius=0.6, deg=c(0, 135), col='cornflowerblue')
#' arc(x=0, y=0, radius=0.3, deg=c(90, 360), col='red', lty='dotted')
#' arc(x=1, y=0, radius=2, deg=c(225, 325), col='green', lwd=2)
#' @export
arc <- function(
	x,
	y,
	radius,
	deg,
	n=1000L,
	...
) {

	### catch errors
	if (length(deg) != 2) {
		stop('Argument "deg" must be a 2-element numeric vector.')
	}

	if (n < 2) {
		n <- 10
		warning('Number of segments used to draw arc must be >1 (argument "n"). Forcing to 10.')
	}

	### calculate inner/outer vertices in polar coordinates
	theta <- seq((-deg[1] + 90) * pi / 180, (-deg[2] + 90) * pi / 180, length.out=n)
	verticesX <- x + radius * cos(theta)
	verticesY <- y + radius * sin(theta)

	### plot
	graphics::lines(verticesX, verticesY, ...)

}

