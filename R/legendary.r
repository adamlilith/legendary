#' omnibus: Fantabulous plotting functions
#'
#' This package contains functions for new kinds of plots and legends.
#'
#' Create an issue on \href{https://github.com/adamlilith/legendary/issues}{GitHub}.
#' @details
#' @section New plots:
#' 		\code{\link{annulus}}: Annulus (a circle with a hole) \cr
#' 		\code{\link{annulusSeg}}: Segmented annuli (segment of a circle with a hole) \cr
#' 		\code{\link{arc}}: Arcs (segment of a circle) \cr
#' 		\code{\link{pancakes}}: Side-by-side bar charts (useful for displaying female/male age structure, for example) \cr
#' 		\code{\link{pies}}: Add a pie chart to a plot \cr
#' 		\code{\link{quadPlot}}: 4-color/corner plot for data with proportional degrees of four categories \cr
#' 		\code{\link{spoke}}: Nodes representing items arranged in a circle with lines drawn between them to represent connections (e.g., variables with high levels of correlation) \cr
#'
#' @section New legends:
#' 		\code{\link{legendBreaks}}: Legend with stacked, colored bars useful for displaying, for example, maps of continuous values that have been thresholded using multiple values \cr
#' 		\code{\link{legendGrad}}: Legend with a color gradient \cr
#' 		\code{\link{legendQuad}}: Legend for data with proportional degrees of four categories (cf. \code{\link{quadPlot}}) \cr
#'
#' @section Auxiliary:
#' 		\code{\link{colorFrom4Vector}}: Transforms a vector of four values to HSV color plus coordinates on a \code{\link{quadPlot}} \cr
#' 		\code{\link{emptyCorner}}: Find the corner of the plot that is the least occupied by plot elements \cr
#' 		\code{\link{getFig}}: Return coordinates for \code{par(fig=xxxx)} for use with \code{layout} function for placing subplots \cr
#' 		\code{\link{labelFig}}: Adds a figure title in the upper left corner of the plot region (as in manuscripts) \cr
#' @docType package
#' @author Adam B. Smith
#' @name legendary
NULL
