# Examine swinging and called strikes.

# Find swinging strikes


mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggplot2)
library(plyr)
library(reshape2)

setwd(dataDir)
# load('atbat09.Rda')
load('pfxData09.Rdat')    # pitchfx '09 data - full monty

# Get rid of unwanted characters in des column (messes up indexing)
pd$des <- gsub('[ (),]', '', pd$des)

cntdf <- count(pd, vars=c('des'))
# cntdf
# des   freq
# 1             Automatic Ball     11
# 2                       Ball 248584
# 3               Ball In Dirt  11718
# 4              Called Strike 123888
# 5                       Foul 114719
# 6        Foul (Runner Going)   2671
# 7                  Foul Bunt   2575
# 8                   Foul Tip   4634
# 9               Hit By Pitch   1563
# 10           In play, no out  30262
# 11           In play, out(s)  86612
# 12           In play, run(s)  16532
# 13               Intent Ball   3662
# 14               Missed Bunt    427
# 15                  Pitchout    482
# 16         Swinging Pitchout      3
# 17           Swinging Strike  56357
# 18 Swinging Strike (Blocked)   3542
# 19            Unknown Strike     19

# firstPitcher <- 400061
# pd.first <- subset(pd, pitcher == firstPitcher, select=c(pitcher, des))
# summarise(pd.first, swingStrikes = sum(des == 'SwingingStrike'))
# count(pd.first, 'pitcher')
# Same as count(pd.first, 'pitcher')
# as.data.frame(table(pd.first$pitcher))

#### 3 ways to create a summary of des for each pitcher.
#### # 1 (creates table)
# p <- pd
# freqTable <- table(p[, c('pitcher', 'des')])
# freqTableDf <- as.data.frame(freqTable)


#### # 2 Alternately (creates data.frame)
ft <- count(pd, c('pitcher', 'des'))
df <- dcast(ft, pitcher ~ des)

#### # 3 xtabs (creates xtabs)
# xt <- xtabs(~ pitcher + des, data = pd)
# xtDf <- as.data.frame(xt)

# How many total pitches?
ft.totPitches <- count(pd, 'pitcher')
names(ft.totPitches)[2] <- 'TotalPitches'
df <- join(df, ft.totPitches, type='left')


#### Create a df of pitcher strikes
#### Plot & model strikes vs total pitches

# Ignore pitchers with < 500 pitches
# How many swinging strikes?

df1 <- subset(df, TotalPitches > 500,
              select=c(pitcher, SwingingStrike, CalledStrike, TotalPitches))
df1 <- mutate(df1, 
              pctSwingStrike = SwingingStrike / TotalPitches,
              pctCalledStrike = CalledStrike / TotalPitches,
              swingVsCalledStrike = SwingingStrike / CalledStrike
              )

# Plot SwingStrike vs Total Pitches
# Extras for  plot
bestfit <- geom_smooth(method = 'lm', se = T, fill = 'red')
bluePoints  <-  geom_point(color = 'blue')
ptitle <- opts(plot.title = theme_text(size = 14, lineheight = 1.2, face = 'bold'))

g1 <- ggplot(df1, aes(TotalPitches, SwingingStrike))
topts <- opts(title = 'Swinging Strikes vs. Total Pitches')
myLabs <- labs(x = 'Total Pitches', y = 'Swinging Strikes')
gg1 <- g1 + myLabs + bluePoints + topts + ptitle + bestfit

g2 <- ggplot(df1, aes(TotalPitches, CalledStrike))
topts <- opts(title = 'Called Strike vs. Total Pitches')
myLabs <- labs(x = 'Total Pitches', y = 'Called Strike')
gg2 <- g2 + myLabs + bluePoints + topts + ptitle + bestfit

df2 <- melt(df1, c('pitcher', 'TotalPitches'))
ggplot(subset(df2, variable %in% c('SwingingStrike', 'CalledStrike')), aes(x=TotalPitches, y=value)) + 
  bluePoints + facet_wrap(~ variable) + bestfit + ptitle +
  labs(x = 'Total Pitches', y = 'Strikes') +
  opts(title = 'Swinging and Called Strikes vs. Total Pitches')

fit.swing <- lm(SwingingStrike ~ TotalPitches - 1, data = df1)
fit.called <- lm(CalledStrike ~ TotalPitches - 1, data = df1)

ggplot(df1, aes(CalledStrike, SwingingStrike)) + bluePoints + bestfit

# Linear model, set intercept to 0
# trainIndices <- sort(sample(1:nrow(df.ss), 0.6 * nrow(df.ss), replace = FALSE))
# testIndices <- which(!1:nrow(df.ss) %in% trainIndices)
# df.train <- df.ss[trainIndices, ]
# df.test <- df.ss[testIndices, ]
# m1 <- lm(SwingingStrike ~ TotalPitches - 1, data = df.train)

m.ss <- lm(SwingingStrike ~ TotalPitches - 1, data = df1)
m.cs <- lm(CalledStrike ~ TotalPitches - 1, data = df1)
df1$residSwingStrike <- resid(m.ss)
df1$residCalledStrike <- resid(m.cs)

# Plot out residuals of SwingStrike & CalledStrike vs TotalPitches
ggplot(df1, aes(TotalPitches, residCalledStrike)) + bluePoints + bestfit
ggplot(df1, aes(TotalPitches, residSwingStrike)) + bluePoints + bestfit

# Top right quadrant is above average SwingStrike & CalledStrikes
ggplot(df1, aes(residSwingStrike, residCalledStrike)) + bluePoints

# Look at fitted values
df1$fitSwingStrike <- m.ss$fitted.values
df1$fitCalledStrike <- m.cs$fitted.values

# look at models
# op <- par(mfrow = c(2, 2), pty = 's')
# plot(m.ss)
# par(op)
plot(m.ss) # plots 4 plots
plot(m.cs)



