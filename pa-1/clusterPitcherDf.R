# Cluster analysis
# Get data from era.R (pitcherDf & pitcherDf.f)

mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggplot2)
library(plyr)
library(reshape2)
library(sqldf)

setwd(dataDir)
load('pitcherDf.Rdat')    # pitchfx '09 data - full monty
setwd(mainDir)

# Do some cluster analysis
# From: http://www.statmethods.net/advstats/cluster.html

# Don't know if I'll use this
# LabelQuantiles <- function(v) {
#   a <- 1 + (v > quantile(v, 0.25)) + (v > quantile(v, 0.5)) + (v > quantile(v, 0.75))
# }

#### Prepare Data ####
# Select columns
# get rid of na's
# scale data (mean = 0, sd = 1)
pitcherDf.toclust <- subset(pitcherDf, ERA < 1.5,
                  select=c(pitcher, pctSwingStrike, ERA, numInningsPitched))
pitcherDf.toclust <- na.omit(pitcherDf.toclust)
mydata <- scale(pitcherDf.toclust[, 2:4])
# mydata <- data.frame(pitcher = pitcherDf.toclust$pitcher, mydata)
mydata <- subset(mydata, select=c(pctSwingStrike, ERA, numInningsPitched))
mydata <- subset(pitcherDf, ERA < 1.5, select=c(pctSwingStrike, ERA, numInningsPitched))
mydata <- na.omit(mydata)
mydata <- scale(mydata)

# k-means
# determine number of clusters
# http://www.statmethods.net/advstats/cluster.html
# Plot within groups sum of squares vs num of clusters to find
#   appropriate number of clusters (look for elbow)
wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var)) # var = 1 because we scaled to sd = 1
minClustSize <- nrow(mydata)
for (i in 2:15) { km <- kmeans(mydata, centers = i)
                  wss[i] <- sum(km$withinss)
                  minClustSize[i] <- min(km$size)
}
toPlot <- data.frame(k = 1:15, wss, minClustSize) 
ggplot(toPlot, aes(k, wss, colour = log10(minClustSize))) + 
  geom_point(size = 5) + geom_smooth(se=F) +
  opts(title = 'Within Groups Sum of Squares vs Number of Clusters') +
  xlab('Number of Clusters') + ylab('Within Group Sum of Squares') + ptitle
# plot(1:15, wss, type='b', xlab = 'Num of Clusters', ylab = 'within groups sum of squares')

# Plot out centers
km <- kmeans(mydata, centers = 5)

kmctr <- as.data.frame(km$centers)
kmctr$k <- 5
ggplot(kmctr, aes(ERA, pctSwingStrike)) + geom_point()

### Side note
### Plot out centers for 2:15 cluster centers
km <- wss[[2]]
kmctrs <- as.data.frame(km$centers)
kmctrs$k <- 2
for (i in 3:15) {
  km <- wss[[i]]
  tmp <- as.data.frame(km$centers)
  tmp$k <- i
  kmctrs <- rbind(kmctrs, tmp)
}
ggplot(kmctrs, aes(ERA, pctSwingStrike)) + 
  geom_point(aes(size = numInningsPitched), colour = 'red4') +
  facet_wrap(~ k) +
  opts(title = 'Cluster Centers vs Number of Clusters')

### How big is each cluster?
for(i in 2:15) print(min(kmeans(mydata, i)$size))
### END Side note

# K-means Cluster analysis
# From above, go with 5 clustes
set.seed(100)
fit <- kmeans(mydata, 5)
kmctr <- as.data.frame(fit$centers)
ggplot(kmctr, aes(ERA, pctSwingStrike)) + geom_point()

mydata <- data.frame(pitcher = pitcherDf.toclust$pitcher, k = fit$cluster, mydata)
pctSwingERA.N.Clust <- data.frame(pitcherDf.toclust, k = fit$cluster, 
                         pctSwingStrikeNorm = mydata$pctSwingStrike,
                         ERANorm = mydata$ERA,
                         numInningsPitchedNorm = mydata$numInningsPitched)
setwd(dataDir)
save(pctSwingERA.N.Clust, file='pctSwingERA.N.Clust.Rda')
setwd(mainDir)

### SAVE DATA FOR FUTURE USE - ESPECIALLY CLUSTER CLASSIFICATIONS
scaledValuesToClusterDf <- mydata
pitcherClusterDf <- subset(mydata, select=c(pitcher, k))
setwd(dataDir)
save(pitcherClusterDf, scaledValuesToClusterDf, file='clusterData.Rda')
setwd(mainDir)
### END SAVE

# add vert & horiz lines showing 1st quartile ERA and 3rd quartile %SwingStrike
goodLimits <- summarise(mydata, 
                        eraQ1 = quantile(ERA, 0.25), 
                        pssQ3 = quantile(pctSwingStrike, 0.75))
vertLine <- geom_vline(x = goodLimits$eraQ1, colour = 'steelblue', alpha = I(0.8))
horizLine <- geom_hline(y = goodLimits$pssQ3, colour = 'steelblue', alpha = I(0.8))

# get cluster means
agg <- aggregate(mydata, by=list(mydata$k), FUN=mean)
aggregate(mydata, by=list(mydata$k), FUN=sd)
### Could do using ddply
# ddply(mydata, 'k', colMeans)

ggplot(mydata, aes(ERA, pctSwingStrike, colour=numInningsPitched)) + 
  geom_point() + facet_wrap(~ k, nrow = 1) +
  vertLine + horizLine +
  ylab('Percent Swing Strike (scaled) - 3rd Quartile') +
  xlab('ERA (scaled) - 1st Quartile') +
  opts(title = 'Results of K-means Clustering on 3 Variables') +
  labs(colour = 'Number Innings\nPitched (scaled)') +
  geom_point(data = agg, colour = 'red', size = 5) # add cluster centers to plot




# Plot Cluster Solution #### optional

# Cluster Plot against 1st 2 principal components
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

fit1 <- kmeans(mydata, 5)
fit2 <- kmeans(mydata, 5)
cluster.stats(d, fit1$cluster, fit2$cluster)