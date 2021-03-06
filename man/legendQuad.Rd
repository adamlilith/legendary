% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legendQuad.r
\name{legendQuad}
\alias{legendQuad}
\title{Creates a "four-color" plot legend for interpreting data with mixtures of four aspects.}
\usage{
legendQuad(
  x,
  y = NULL,
  inset = 0,
  width = 0.2,
  height = 0.2,
  labels = LETTERS[1:4],
  labelAdj = 1,
  cols = c("white", "cyan", "black", "red"),
  n = 25,
  border = par("fg"),
  title = "",
  titleAdj = c(0.5, 0.9),
  aspect = FALSE,
  swatchAdjX = c(0.15, 0.85),
  swatchAdjY = c(0.25, 0.85),
  boxBg = par("bg"),
  boxBorder = par("fg"),
  ...
)
}
\arguments{
\item{x}{Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the top left of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'}, \code{'bottomright'}, \code{'top'}, \code{'bottom'}, \code{'left'}, or \code{'right'}).}

\item{y}{Numeric or \code{NULL}. Y-coordinate of the top left corner of the legend.}

\item{inset}{Numeric. If \code{x} is a word describing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertical offset.}

\item{width}{Numeric. Scaling factor for box width.}

\item{height}{Numeric. Scaling factor for box height.}

\item{labels}{Character vector used to name the four corners of the color swatch in this order: bottom left, top left, top right, bottom right. Leave as \code{NULL} to skip displaying labels.}

\item{labelAdj}{Numeric, usually between 0 and 1. Position of corner labels relative to the corners of the swatch.}

\item{cols}{Character list. Names of four colors to be used to create a gradient to fill the color swatch. The first color will be assigned to the lower left corner, the second to the upper left, third to upper right, and fourth to lower right. Note that not every color combination produces a map with a unique color at each coordinate of the color swatch.}

\item{n}{Positive integer, number of squares used to approximate a smooth color map in the swatch. This is the number of cells along each side of the swatch. Higher values create less "blocky" swatches but increase drawing time.}

\item{border}{Character or integer. Name (or integer code) of color to use to draw border of the color swatch.  Leave as \code{NULL} to skip drawing a border.}

\item{title}{Character or \code{NULL}. Name of title for the legend.}

\item{titleAdj}{Two numeric values between 0 and 1. Position of the legend relative to the container box. The first pertains to horizontal positioning and the second vertical positioning.}

\item{aspect}{Logical, if \code{TRUE} then the height of the color swatch is scaled by \code{swatchAdjX} so that the swatch is square (and \code{swatchAdjY} is ignored).  If \code{FALSE} then the height is determined by \code{swatchAdjY}.}

\item{swatchAdjX}{Two numeric values between 0 and 1. Size of the color swatch in the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.}

\item{swatchAdjY}{Two numeric values between 0 and 1. Size of the swatch in the y-dimension as a proportion of the container box size. The first pertains to the top of the bar and the second the bottom. Alternatively, if \code{aspect} is \code{TRUE} then the first value of \code{swatchAdjY} is used to place the top of the swatch but the bottom is located such that the swatch is square (i.e., the second value of \code{swatchAdjY} is ignored).}

\item{boxBg}{Character or integer. Name (or integer code) of color to use to use for box containing legend. Leave as \code{NULL} to not draw a box.}

\item{boxBorder}{Character or integer. Name (or integer code) of color to use to use for box border containing legend. Leave as \code{NULL} to not draw a box border.}

\item{...}{Arguments to pass to \code{\link{plot}}, \code{\link[graphics]{polygon}}, or \code{\link[graphics]{text}}.}
}
\value{
Nothing (side effect is to add a legend to an existing graphics device).
}
\description{
This function adds a legend to an existing plot. The legend is typically a square with each corner assigned a different color. The closer an object in the main plot is to one of four "assignments" (e.g., populations), the more the corresponding color is shown, with assignments of intermediate value displaying as a mixture of colors. The function first draws a "containing" box then a square "color swatch" with a color gradient inside. A legend title and labels for the four corners can be added.
}
\examples{
data(religion)
head(religion)
religion$others <- rowSums(religion[ , c('unaffiliated', 'hindu', 'folk', 'other', 'jewish')])
religs <- c('christian', 'muslim', 'buddhist', 'others')
par(mfrow=c(1, 2))
plotQuad(religion[ , religs], 'points', background=FALSE, main='Religion by Country')
legendQuad('top', inset=0.01, labels=colnames(relig))
}
\seealso{
\code{\link[graphics]{legend}}, \code{\link[legendary]{legendGrad}},
}
