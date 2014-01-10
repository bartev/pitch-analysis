# Pitcher summaries

mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggplot2)
library(plyr)
library(reshape2)
library(sqldf)
library(scales) # for alpha ?

setwd(dataDir)
load('pitcherDf.Rdat')    # pitchfx '09 data - full monty
load('pfxData09.Rdat')    # pitchfx '09 data - full monty
setwd(mainDir)
source('fnUtilityFunctions.R')

# Keep top 6 pitches
# Arrange(count(pd, 'pitch_type'), -freq)
# pitch_type   freq
# 1          FF 345788
# 2          SL 118963
# 3          CH  83512
# 4          CU  65605
# 5          FA  30413
# 6          FT  26530
# 7          FC  18462
# 8          SI  11842
# 9          IN   3549
# 10         KN   2474
# 11         PO    485
# 12         FS    441
# 13         UN    159
# 14         SC     25
# 15         AB     11
# 16         EP      2
pitchesToKeep <- c('FF', 'SL', 'CH', 'CU', 'FA', 'FT')

df <- subset(pd, pitch_type %in% pitchesToKeep, 
             select=c(pitcher, pitch_type, pfx_x, pfx_z, spin_dir, spin_rate))
df <- mutate(df, SpinAngle = (spin_dir + 270) %% 360 - 180)
# Positive SpinAngle indicates top spin, negative ~ bottom spin (e.g. fast ball)
plInfo <- subset(pl, select=c(id, throws, bats))
names(plInfo)[1] <- 'pitcher'
df <- join(df, plInfo)


# Function to get mean/sd displacement/spin in horiz/vert directions
# Apply this to each pitcher & pitch_type using ddply
f.avgDisp <- function(d) {
  summarise(d, 
            avgVertDisp = mean(pfx_z, na.rm = TRUE),
            sdVertDisp = sd(pfx_z, na.rm = TRUE),

            avgHorizDisp = mean(pfx_x, na.rm = TRUE),
            sdHorizDisp = sd(pfx_x, na.rm = TRUE)
            )
}
f.avgSpin <- function(d) {
  summarise(d, 
            avgVertSpin = mean(Sin(SpinAngle) * spin_rate, na.rm = TRUE),
            sdVertSpin = sd(Sin(SpinAngle) * spin_rate, na.rm = TRUE),
            
            avgHorizSpin = mean(Cos(SpinAngle) * spin_rate, na.rm = TRUE),
            sdHorizSpin = sd(Cos(SpinAngle) * spin_rate, na.rm = TRUE)
            )
}
f.avgSpinAngle <- function(d) {
  summarise(d, 
            avgSpinAngle = mean(SpinAngle),
            sdSpinAngle = sd(SpinAngle)
            )
}
# Create data.frame of pitcher, pitch_type and mean/sd disp/spin in horiz/vert directions
avgDispType <- ddply(df, c('pitcher', 'pitch_type', 'throws'), f.avgDisp)
avgSpinType <- ddply(df, c('pitcher', 'pitch_type', 'throws'), f.avgSpin)
avgSpinAngleType <- ddply(df, c('pitcher', 'pitch_type', 'throws'), f.avgSpinAngle)
moltenDisp <- melt(avgDispType, c('pitcher', 'pitch_type', 'throws'))
moltenSpin <- melt(avgSpinType, c('pitcher', 'pitch_type', 'throws'))
moltenSpinAngle <- melt(avgSpinAngleType, c('pitcher', 'pitch_type', 'throws'))

setwd(dataDir)
save(avgDispType, avgSpinType, avgSpinAngleType, moltenDisp, moltenSpin, moltenSpinAngle, file = 'pitcherSummaries.Rda')
setwd(mainDir)

# Try plotting
# Not so useful
ggplot(moltenDisp, aes(x=pitch_type, y=value)) + geom_boxplot() + facet_grid(variable ~ throws) +
  opts(title = 'Mean & SD Displacement due to Spin vs Pitch type')
ggplot(subset(moltenDisp, variable %in% c('avgVertDisp', 'avgHorizDisp')), aes(x = variable, y = value, group = pitcher)) + 
  geom_point() + 
  facet_grid(pitch_type ~ throws) + geom_line(aes(group = pitcher)) +
  geom_hline(y=0, colour='blue') +
  geom_boxplot(aes(group = variable), colour = 'red') +
  opts(title = 'Mean Displacement due to Spin vs Pitch type')

ggplot(moltenSpin, aes(x=pitch_type, y=value)) + geom_boxplot() + facet_grid(variable ~ throws)

ggplot(moltenSpinAngle, aes(x=pitch_type, y=value)) + geom_boxplot() + facet_grid(throws ~ variable) +  
  geom_hline(y=0, colour='blue') + scale_y_continuous(limits=c(-180, 180), breaks = seq(-180, 180, 90)) +
  opts(title = 'Mean Spin Angle per Pitcher vs Pitch type') +
  ylab('Spin Angle') + xlab('Pitch Type')




# Look at pitch_type vs b/s/x
# SPLIT - One pitcher only
df1 <- subset(pd, pitcher == 400061 & pitch_type %in% pitchesToKeep, 
              select=c(batter, type, pitch_type, pfx_x, pfx_z, spin_dir, spin_rate))
df1.c <- count(df1, c('type', 'pitch_type'))
df1.c <- ddply(df1.c, 'pitch_type', mutate, propFreq = freq / sum(freq))
df1.pt <- dcast(df1.c, pitch_type ~ type, value.var='propFreq')  # prop table
df1.ft <- dcast(df1.c, pitch_type ~ type, value.var='freq')  # freq table

# >df1.pt
# pitch_type    B    S    X
# 1         CH 0.33 0.39 0.28
# 2         CU 0.43 0.36 0.20
# 3         FF 0.28 0.52 0.20
# 4         SL 0.31 0.48 0.21
# > df1.ft
# pitch_type   B   S   X
# 1         CH  57  68  48
# 2         CU 210 178 100
# 3         FF 434 814 311
# 4         SL 125 196  86
ggplot(df1.c, aes(y=propFreq, x=type)) + geom_point(size=3) + facet_wrap(~ pitch_type, nrow = 1)

## COMBINE -- look at multiple pitchers
df2 <- subset(pd, pitch_type %in% pitchesToKeep,
              select=c(pitcher, batter, type, pitch_type, pfx_x, pfx_z, spin_dir, spin_rate))
df2.c <- count(df2, c('pitcher', 'type', 'pitch_type'))
df2.c <- ddply(df2.c, c('pitcher', 'pitch_type'), mutate, propFreq = freq / sum(freq))
df2.pt <- ddply(df2.c, 'pitcher', function(x) dcast(x, pitch_type ~ type, sum, value.var='propFreq'))  # prop table
df2.ft <- ddply(df2.c, 'pitcher', function(x) dcast(x, pitch_type ~ type, sum, value.var='freq'))  # freq table
ggplot(df2.c, aes(type, propFreq)) + geom_boxplot() + geom_point( alpha = 0.1) + facet_wrap(~ pitch_type, nrow = 1)

# Omit lines where too few pitches (freq <= 20)
tmp <- count(df2.c, c('pitcher', 'pitch_type'))
tmp2 <- join(subset(tmp, freq > 20, select=c(pitcher, pitch_type)), df2.c)
# Plot PropFreq vs B/S/X for various pitch_types
ggplot(tmp2, aes(type, propFreq)) + geom_boxplot() + geom_point( alpha = 0.1) + facet_wrap(~ pitch_type, nrow = 1) +
  xlab('Result of Pitch') + ylab('Proportional Frequency (Omit rows with < 20 pitches)') +
  opts(title = 'Frequency of B/S/X for Various Pitch Types')



# Add pitcher Good/Bad quality from clustering
tmp2$pitcher <- as.factor(tmp2$pitcher)
pitchTypePropFreq <- join(tmp2, pq)
ggplot(subset(pitchTypePropFreq, !is.na(pitchTypePropFreq$best)), aes(type, propFreq)) + 
  geom_boxplot() + geom_point( alpha = 0.1) + facet_grid(pitch_type ~ best) +
  opts(title = 'Prop Frequency vs Pitcher Quality as Based On Clustering')


#### NOT GOOD ####
# Try a model
# for each pitcher, have freq of B/S/X for each pitch_type
dftm <- pitchTypePropFreq  # df to model
dftm$best <- as.factor(dftm$best)

trainIn <- sample(1:nrow(dftm), 0.6 * nrow(dftm), replace = FALSE)
testIn <- which(! 1:nrow(dftm) %in% trainIn)

fit <- glm(best ~ pitch_type + propFreq, data=dftm[trainIn, ], family = binomial)



