# What drives singles?


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

# get all hits
ah <- subset(pd, type == 'X' & des %in% c('In play, run(s)', 'In play, no out'),
             select=c(pitcher, batter, ubnum, num, b, s, x, y, on_1b, on_2b, on_3b, start_speed, end_speed,
                      pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az,
                      break_y, break_angle, break_length, pitch_type, spin_dir, spin_rate, szBot, szTop,
                      pxScale, pzScale, sZone, inZone, League, count, pitchNum, home))
ab.sgl <- subset(ab,,
                 select=c(pitcher, batter, ubnum, half, inning, p_throws, stand, event, pAhead, runsOnPitcher))

ab.sgl$ubnum <- as.factor(ab.sgl$ubnum)
ah$pitcher <- as.factor(ah$pitcher)

sgls <- join(ah, ab.sgl)

sgls <- mutate(sgls, SpinAngle = (spin_dir + 270) %% 360 - 180)
sgls <- mutate(sgls, VertSpin = Sin(SpinAngle) * spin_rate,
                     HorizSpin = Cos(SpinAngle) * spin_rate,
                     VertDisp = pfx_z,
                     HorizDisp = pfx_x,
                     outcome = (event == 'Single')
                     )
sgls$outcome <- as.factor(sgls$outcome)

sgls.num <- subset(sgls,
                   select=c(start_speed, end_speed, pfx_x, pfx_z, x0, z0, vx0, vy0, vz0,
                            ax, ay, az, break_y, break_angle, break_length,
                            szBot, szTop, pxScale, pzScale, sZone, pitchNum, SpinAngle))

factanal(~ start_speed + end_speed + pfx_x + pfx_z + pxScale + pzScale + 
  SpinAngle,
         data=subset(sgls, pitch_type=='FF'), factors=3)



ss <- subset(sgls, pitch_type == 'FF')
cdplot(outcome ~ pxScale, data=ss)
cdplot(outcome ~ pzScale, data=ss)
cdplot(outcome ~ SpinAngle, data=ss)  # most spin angles beteween -180 & 0
  ggplot(ss, aes(SpinAngle)) + geom_bar()

cdplot(outcome ~ VertSpin, data=ss)
cdplot(outcome ~ HorizSpin, data=ss)

cdplot(outcome ~ b, data=ss)
cdplot(outcome ~ s, data=ss)
cdplot(outcome ~ on_1b, data=ss)
cdplot(outcome ~ on_2b, data=ss)
cdplot(outcome ~ on_3b, data=ss)
cdplot(outcome ~ start_speed, data=ss)
  ggplot(ss, aes(start_speed)) + geom_bar()

cdplot(outcome ~ end_speed, data=ss)
  ggplot(ss, aes(end_speed)) + geom_bar()

cdplot(outcome ~ VertDisp, data=ss)
ggplot(ss, aes(VertDisp)) + geom_bar()  # decreased VertDisp -> increased likelihood of Single
cdplot(outcome ~ HorizDisp, data=ss)


cdplot(outcome ~ x0, data=ss)
cdplot(outcome ~ y0, data=ss)
cdplot(outcome ~ z0, data=ss)
ggplot(ss, aes(z0)) + geom_bar()

cdplot(outcome ~ vx0, data=ss)
  ggplot(ss, aes(vx0)) + geom_bar()
cdplot(outcome ~ vy0, data=ss)
cdplot(outcome ~ vz0, data=ss)
  ggplot(ss, aes(vz0)) + geom_bar()

cdplot(outcome ~ break_y, data=ss)
cdplot(outcome ~ break_angle, data=ss)
cdplot(outcome ~ break_length, data=ss)
  ggplot(ss, aes(break_length)) + geom_bar()

cdplot(outcome ~ spin_dir, data=ss)
  ggplot(ss, aes(spin_dir)) + geom_bar()
cdplot(outcome ~ spin_rate, data=ss)
cdplot(outcome ~ szBot, data=ss)
cdplot(outcome ~ szTop, data=ss)
cdplot(outcome ~ pxScale, data=ss) # Abs(pxScale) closer to 2 is better than 0
  ggplot(ss, aes(pxScale)) + geom_bar()
cdplot(outcome ~ pzScale, data=ss) # Lower pzScale is better than higher
  ggplot(ss, aes(pzScale)) + geom_bar()
cdplot(outcome ~ sZone, data=ss) # Bottom of strike zone is better than higher
cdplot(outcome ~ inZone, data=ss)
cdplot(outcome ~ League, data=ss)
cdplot(outcome ~ count, data=ss)
ggplot(ss, aes(outcome, fill = outcome)) + geom_bar() + facet_wrap(~ count, ncol=3) +
  opts(title='Frequency of Hits Resulting in Singles (When not Out)') +
  xlab('Single') + ylab('Frequency')
ggplot(ss, aes(outcome, fill = outcome)) + geom_bar() +
  opts(title='Frequency of Hits Resulting in Singles (When not Out)') +
  xlab('Single') + ylab('Frequency')

cdplot(outcome ~ pitchNum, data=ss)
  ggplot(ss, aes(pitchNum)) + geom_bar()
cdplot(outcome ~ home, data=ss)
cdplot(outcome ~ half, data=ss)
cdplot(outcome ~ stand, data=ss)
cdplot(outcome ~ event, data=ss) # levels(ss$event)[30] => 'Single', go figure!

cdplot(outcome ~ pAhead, data=ss) # more likely to hit singles if pAhead > 0
ggplot(ss, aes(pAhead)) + geom_bar()

cdplot(outcome ~ runsOnPitcher, data=ss) # looks like more likely to get single if fewer runs on pitcher
ggplot(ss, aes(runsOnPitcher)) + geom_bar()


### rpart
fit <- rpart(outcome ~ num + b + s + type + x + y + on_1b + on_2b + on_3b +
  start_speed + end_speed + pfx_x + pfx_z + x0 + y0 + z0 + vx0 + vy0 + vz0 +
  ax + ay + az + break_y + break_angle + break_length +
  pitch_type + spin_dir + spin_rate + szBot + szTop + pxScale + pzScale +
  sZone + inZone + League + count + pitchNum + home + half + inning +
  p_throws + stand + event + pAhead + runsOnPitcher +
  SpinAngle + VertSpin + HorizSpin + VertDisp + HorizDisp,
             method = 'class', data = sgls)

fit <- rpart(outcome ~ pxScale + pzScale + SpinAngle + 
  VertSpin + HorizSpin + VertDisp + HorizDisp,
             method = 'class', data = subset(sgls, pitch_type == 'FF'))

print(fit)
printcp(fit)
plotcp(fit)
summary(fit)


### haven't done yet
## k-means clustering
mydata <- sgls
wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var)) # var = 1 because we scaled to sd = 1
minClustSize <- nrow(mydata)
for (i in 2:15) { km <- kmeans(mydata, centers = i)
                  wss[i] <- sum(km$withinss)
                  minClustSize[i] <- min(km$size)
}
toPlot <- data.frame(k = 1:15, wss, minClustSize) 
ggplot(toPlot, aes(k, wss, colour = log10(minClustSize))) + 
  geom_point(size = 5) + geom_smooth(se=F) +
  opts(title = 'Within Groups Sum of Squares vs Number of Clusters')
# plot(1:15, wss, type='b', xlab = 'Num of Clusters', ylab = 'within groups sum of squares')

# Plot out centers
km <- kmeans(mydata, centers = 5)

kmctr <- as.data.frame(km$centers)
kmctr$k <- 5
ggplot(kmctr, aes(ERA, pctSwingStrike)) + geom_point()

