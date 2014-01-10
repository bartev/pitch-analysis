MinMaxQuant <- function(df, col) {
  # find min, max, q1, q2, q3 for a df and col
  vec <- df[, col]
  quant <- quantile(vec, c(0.25, 0.5, 0.75), na.rm=TRUE)
  names(quant) <- c('q1', 'q2', 'q3')
  minMax <- c(min=min(vec, na.rm=TRUE), max=max(vec, na.rm=TRUE))
  result <- c(minMax, quant)
  # add colname to results colnames
  names(result) <- paste(col, names(result), sep="_")
  return(result)
}

## Example with ddply
# test2 <- ddply(pd, c('batter'), function(x) MinMaxQuant(x, 'sz_top'), .progress='text')

# names(szBotMMQ)[2:ncol(szBotMMQ)] <- paste('szBot', c('min', 'max', 'q1', 'q2', 'q3'), sep='_')
# names(szTopMMQ)[2:ncol(szTopMMQ)] <- paste('szTop', c('min', 'max', 'q1', 'q2', 'q3'), sep='_')


multiplot <- function(..., plotlist=NULL, cols) {
  # from
  # http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # make the panel
  plotCols = cols  # Number of columns of plots
  plotRows = ceiling(numPlots / plotCols) # Number of rows needed calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling( i / plotCols )
    curCol = (i - 1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}

Sin <- function(theta) sin(theta * pi / 180)
Cos <- function(theta) cos(theta * pi / 180)