# pitcher quality'

setwd(dataDir)
load('clusterData.Rda')
setwd(mainDir)

pq <- pitcherClusterDf
pq$best <- 'NotBest'
pq$best[which(pitcherClusterDf$k == 2)] <- 'Best'
levels(pq$best) <- c('Best', 'NotBest')
levels(pq$best)