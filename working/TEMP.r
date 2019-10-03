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
