# legendary
<img align="right" src="arthur_byPyle1903.jpg" height="190"/>
This package contains functions for new kinds of plots and legends.

You can install this package in R using these commands:

`install.packages('devtools') # if you haven't done this already`  
`library(devtools)`  
`install_github('adamlilith/legendary')`  

NB: If for some reason these commands don't work, you can install the package(s) by downloading the latest zip/tar file from the `zipTarFiles` directory and installing the package(s) manually.

## New plots ##
* `annulus`: Annulus (a circle with a hole)
* `annulusSeg`: Segmented annuli (segments of circles with holes)
* `arc`: Arcs (segments of circles)
* `pancakes`: Side-by-side bar charts (useful for displaying female/male age structure, for example)
* `pies`: Add a pie chart to a plot
* `quadPlot`: 4-color/corner plot for data with proportional degrees of four categories
* `spoke`: Nodes representing items arranged in a circle with lines drawn between them to represent connections (e.g., variables with high levels of correlation)

## New legends ##
* `legendBreaks`: Legend with stacked, colored bars useful for displaying, for example, maps of continuous values that have been thresholded using multiple values
* `legendGrad`: Legend with a color gradient
* `legendQuad`: Legend for data with proportional degrees of four categories (cf. `quadPlot`)

## Auxiliary ###
* `colorFrom4Vector`: Transforms a vector of four values to HSV color plus coordinates on a `quadPlot`
* `emptyCorner`: Find the corner of the plot that is the least occupied by plot elements
* `getFig`: Return coordinates for `par(fig=xxxx)` for use with `layout` function for placing subplots
* `labelFig`: Adds a figure title in the upper left corner of the plot region (as in manuscripts).
