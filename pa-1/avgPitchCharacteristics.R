# For each pitcher, look at 
# P(Single | pitch_type)
# pitchCharacteristics (mean, sd, etc.) | pitch_type

mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggdendro)
library(ggplot2)
library(hexbin)
library(MASS)
library(plyr)
library(reshape2)
library(rpart)
library(scales) # for alpha ?
library(sqldf)


setwd(dataDir)
load('clusterData.Rda')
load('pitcherSummaries.Rda')
load('pitcherDf.Rdat')    # pitchfx '09 data - full monty
load('pfxData09.Rdat')    # pitchfx '09 data - full monty
setwd(mainDir)

source('fnUtilityFunctions.R')

# Full data set
ah <- subset(pd, type == 'X' & des %in% c('In play, run(s)', 'In play, no out'),
             select=c(pitcher, batter, ubnum, num, b, s, x, y, on_1b, on_2b, on_3b, start_speed, end_speed,
                      pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az,
                      break_y, break_angle, break_length, pitch_type, spin_dir, spin_rate, szBot, szTop,
                      pxScale, pzScale, sZone, inZone, League, count, pitchNum, home))
eventsToKeep  <- c('Single', 'Double', 'Triple', 'Home Run')
ab.sgl <- subset(ab, event %in% eventsToKeep,
                 select=c(pitcher, batter, ubnum, half, inning, p_throws, stand, event, pAhead, runsOnPitcher))

ab.sgl$ubnum <- as.factor(ab.sgl$ubnum)
ah$pitcher <- as.factor(ah$pitcher)

sgls <- join(ah, ab.sgl, type = 'inner')

sgls <- mutate(sgls, SpinAngle = (spin_dir + 270) %% 360 - 180)
sgls <- mutate(sgls, VertSpin = Sin(SpinAngle) * spin_rate,
               HorizSpin = Cos(SpinAngle) * spin_rate,
               VertDisp = pfx_z,
               HorizDisp = pfx_x,
               outcome = (event == 'Single')
               )
sgls$outcome <- as.factor(sgls$outcome)
sgls$throws <- sgls$p_throws

# SPLIT
# one pitcher
ss <- subset(sgls, pitcher == '400061')
ss <- subset(sgls, pitch_type %in% pitchesToKeep)
# Calculate freq, P(Event | Pitch_type) & P(Pitch_type | Event)
c.ss <- count(ss, c('pitcher', 'pitch_type', 'event')) # Freqency of outcome for each pitch_type
c.ss <- ddply(c.ss, c('pitcher', 'pitch_type'), function(df) mutate(df, PEvent = freq / sum(freq)))
c.ss <- ddply(c.ss, c('pitcher', 'event'), function(df) mutate(df, PPitchType = freq / sum(freq)))



## Find Mean & SD of speed & location variables
# add start_speed, end_speed, (end_speed - start_speed), pxScale, pzScale
f.speed.mean <- function(d) {
  summarise(d, 
            avgStartSpeed = mean(start_speed),
            avgEndSpeed = mean(end_speed),
            avgDiffSpeed = mean(end_speed - start_speed),
            avgPxScale = mean(pxScale),
            avgPzScale = mean(pzScale)
            )
}
f.speed.sd <- function(d) {
  summarise(d, 
            sdStartSpeed = sd(start_speed),
            sdEndSpeed = sd(end_speed),
            sdDiffSpeed = sd(end_speed - start_speed),
            sdPxScale = sd(pxScale),
            sdPzScale = sd(pzScale)
            )
}
# Create data.frame of pitcher, pitch_type and mean/sd disp/spin in horiz/vert directions
avgSpeeds <- ddply(sgls, c('pitcher', 'pitch_type', 'throws'), f.speed.mean)
sdSpeeds <-  ddply(sgls, c('pitcher', 'pitch_type', 'throws'), f.speed.sd)

moltenSpeedMean <- melt(avgSpeeds, c('pitcher', 'pitch_type', 'throws'))
moltenSpeedSd <- melt(sdSpeeds, c('pitcher', 'pitch_type', 'throws'))


# Get pitcher summaries from pitcherSummaries.Rda (mean, sd, Disp, Spin, etc)
# From pitcherSummaries.R
setwd(dataDir)
load('pitcherSummaries.Rda')
# save(avgDispType, avgSpinType, avgSpinAngleType, 
# moltenDisp, moltenSpin, moltenSpinAngle, file = 'pitcherSummaries.Rda')
setwd(mainDir)

## Combine all molten df's
moltenAll <- rbind(moltenDisp, moltenSpin, moltenSpinAngle)
moltenAll$pitcher <- as.factor(moltenAll$pitcher)
moltenAll$throws <- as.factor(moltenAll$throws)
moltenAll <- rbind(moltenAll, moltenSpeedMean, moltenSpeedSd)


# Get rid of rows with NA values
# Only keep top 6 pitches
pitchesToKeep <- c('FF', 'SL', 'CH', 'CU', 'FA', 'FT')
moltenAll <- subset(moltenAll, !is.na(value) & pitch_type %in% pitchesToKeep)

# Normalize to mean = 0, sd = 1
moltenAll.Norm <- ddply(moltenAll, c('pitch_type', 'variable', 'throws'), 
                        function(df) mutate(df, MeanValue = mean(value), 
                                            DiffValue = value - MeanValue,
                                            SDValue = sd(value),
                                            NormValue = DiffValue / SDValue))


ggplot(moltenAll.Norm, aes(variable, NormValue, colour=pitch_type), alpha=0.2) + 
  facet_wrap(~ throws, nrow=2) + geom_boxplot() +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0)) +
  opts(title = 'Normalized Spin Effects vs Pitch Type') + 
  ylab('Normalized Value (mean = 0, sd = 1)') +
  xlab('')


## look at pitcher/pitch_type/throws vs everything else
dma <- dcast(moltenAll.Norm, pitcher + pitch_type + throws ~ variable)
j.dma <- join(c.ss, dma, type = 'inner')

## rpart?
j.dma <- mutate(j.dma, 
                Sgl = event == 'Single',
                Dbl = event == 'Double',
                Tpl = event == 'Triple',
                HR = event == 'Home Run')
j.dma$Sgl <- as.factor(j.dma$Sgl)

fit <- rpart(Sgl ~ pitch_type + throws + 
  avgVertDisp + sdVertDisp + avgHorizDisp + sdHorizDisp +
  avgVertSpin + sdVertSpin + avgHorizSpin + sdHorizSpin +
  avgSpinAngle + sdSpinAngle +
  avgStartSpeed + sdStartSpeed + avgEndSpeed + sdEndSpeed + avgDiffSpeed + sdDiffSpeed +
  avgPxScale + sdPxScale + avgPzScale + sdPzScale, 
             method = 'class', data = j.dma)

print(fit)
printcp(fit)
plotcp(fit)
summary(fit)

plot(fit, uniform = T)
text(fit, use.n=T, all = T, cex = 0.8)
## rpart didn't show much

## logistic regression?
full.mod <- glm(Sgl ~ 
  pitch_type:(avgVertDisp + sdVertDisp + avgHorizDisp + sdHorizDisp +
  avgVertSpin + sdVertSpin + avgHorizSpin + sdHorizSpin +
  avgSpinAngle + sdSpinAngle +
  avgStartSpeed + sdStartSpeed + avgEndSpeed + sdEndSpeed + sdDiffSpeed +
  avgPxScale + sdPxScale + avgPzScale + sdPzScale), 
                family = binomial, data = subset(j.dma, throws == 'R'))
### logistic regression showed nothing

### linear regression?
trainIndices <- sample(1:nrow(j.dma), 0.6 * nrow(j.dma), replace = F)
trainSet <- subset(j.dma[trainIndices, ], throws == 'R' & event == 'Single')
testSet <- subset(j.dma[!trainIndices, ], throws == 'R' & event == 'Single')

full.mod <- lm(PEvent ~ pitch_type:(avgVertDisp + sdVertDisp + avgHorizDisp + sdHorizDisp +
  avgVertSpin + sdVertSpin + avgHorizSpin + sdHorizSpin +
  avgSpinAngle + sdSpinAngle +
  avgStartSpeed + sdStartSpeed + avgEndSpeed + sdEndSpeed +
  avgPxScale + sdPxScale + avgPzScale + sdPzScale), 
               data = subset(trainSet))
              
red.mod <- step(full.mod, direction = 'backward')
summary(full.mod)
summary(red.mod)
### linear regression showed very poor fit.

### Can I cluster the pitchers?
j.dma.omit <- na.omit(j.dma)
j.dma.omit <- subset(j.dma.omit, event == 'Single' & throws == 'R')
# remove some columns
toRemove = c('pitcher', 'freq', 'PEvent', 'PPitchType', 'Sgl', 'Dbl', 'Tpl', 'HR', 
             'pitch_type', 'event', 'throws', 
             'avgVertSpin', 'avgHorizSpin', 'sdVertSpin', 'sdHorizSpin', 
             'avgStartSpeed', 'sdStartSpeed',
             'sdPxScale', 'sdPzScale')
j.dma.omit <- j.dma.omit[, !names(j.dma.omit) %in% toRemove]

mydata <- j.dma.omit
wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var), na.rm = T) # var = 1 because we scaled to sd = 1
minClustSize <- nrow(mydata)
for (i in 2:15) { km <- kmeans(mydata, centers = i)
                  wss[i] <- sum(km$withinss)
                  minClustSize[i] <- min(km$size)
}
toPlot <- data.frame(k = 1:15, wss, minClustSize) 
ggplot(toPlot, aes(k, wss, colour = log10(minClustSize))) + 
  geom_point(size = 5) + geom_smooth(se=F) +
  opts(title = 'Within Groups Sum of Squares vs Number of Clusters')


# Plot out centers
set.seed(100)

km <- kmeans(mydata, centers = 6)

kmctr <- as.data.frame(km$centers)
kmctr$k <- 5
ggplot(kmctr, aes(avgVertDisp, avgHorizDisp)) + geom_point()
ggplot(kmctr, aes(avgVertDisp, avgHorizDisp)) + geom_point()
ggplot(kmctr, aes(avgVertDisp, avgHorizDisp)) + geom_point()
ggplot(kmctr, aes(avgVertDisp, avgHorizDisp)) + geom_point()
ggplot(kmctr, aes(avgVertDisp, avgHorizDisp)) + geom_point()
ggplot(kmctr, aes(avgVertDisp, avgHorizDisp)) + geom_point()


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



### QUESTION:
### Given pitcher stats, predict P(Outcome | pitch_type)

