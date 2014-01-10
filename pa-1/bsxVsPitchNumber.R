# B/S/X vs pitch number
# how does performance change as more pitches against batter

mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggplot2)
library(plyr)
library(reshape2)
library(sqldf)

setwd(dataDir)
load('pitcherDf.Rdat')    # pitchfx '09 data - full monty
load('pfxData09.Rdat')    # pitchfx '09 data - full monty
setwd(mainDir)

pnumDf <- subset(pd, select=c(pitcher, type, pitchNum, count, home, pitch_type))
pnumDf$pitcher <- as.factor(pnumDf$pitcher)

# Using xtabs & prop.table is good, but harder to filter out freq > 10
# t <- xtabs(~ type + pitchNum, data = pnumDf)
# propT <- prop.table(t, 2)
# propT <- as.data.frame(propT)

# Plot B/S/X proportional frequency vs pitch Number
bsxDf <- count(pnumDf, c('type', 'pitchNum'))
propBsx <- ddply(bsxDf, 'pitchNum', function(x) { mutate(x, Freq = freq / sum(freq)) })
ggplot(subset(propBsx, freq > 10), aes(pitchNum, Freq)) + 
  geom_smooth(method = 'lm', aes(group=type), fill = 'red') + geom_point() +
  facet_wrap( ~ type) +
  opts(title = 'Result of Pitch vs. Pitch Number') + ptitle +
  labs(x = 'Pitch Number Per Batter', y = 'Frequency')

# Now separate starters from relievers
pnumDf.sr <- join(pnumDf, subset(pitcherDf, select=c(pitcher, StartRelief)))
bsxDf.sr <- count(pnumDf.sr, c('type', 'pitchNum', 'StartRelief'))
propBsx.sr <- ddply(bsxDf.sr, c('pitchNum', 'StartRelief'), function(x) { mutate(x, Freq = freq / sum(freq)) })
ggplot(subset(propBsx.sr, freq > 10), aes(pitchNum, Freq, colour = StartRelief)) + 
  geom_smooth(method = 'lm', aes(group=c(type, StartRelief)), se=F) +
  geom_point(size = 3) +
  facet_wrap( ~ type) +
  opts(title = 'Result of Pitch vs. Pitch Number (Separate Starter/Reliever)') + ptitle +
  labs(x = 'Pitch Number Per Batter', y = 'Frequency')



# Plot B/S/X proportional freq vs pitch number conditioned on the current COUNT
bsxDf2 <- count(pnumDf, c('type', 'pitchNum', 'count'))
propBsx2 <- ddply(bsxDf2, c('pitchNum', 'count'), function(x) { mutate(x, Freq = freq / sum(freq)) })
ggplot(subset(propBsx2, freq > 10), aes(pitchNum, Freq, colour = type)) + 
  geom_point() + facet_wrap(~ count, ncol = 3) +
  opts(title = 'Proportion of Pitches Resulting in B/S/X vs Pitch Number by Count')

# Plot B/S/X prop freq vs pitch number conditioned on pitch_type
bsxDf3 <- count(pnumDf, c('type', 'pitchNum', 'pitch_type'))
propBsx3 <- ddply(bsxDf3, c('pitchNum', 'pitch_type'), function(x) { mutate(x, Freq = freq / sum(freq)) })
# Remove some pitches
pitchesToRemove <- c('AB', 'FS', 'IN', 'PO', 'SI', 'UN', 'EP', 'SC')
propBsx3.most <- subset(propBsx3, !pitch_type %in% pitchesToRemove & freq > 10)
ggplot(propBsx3.most, aes(pitchNum, Freq, colour = type)) + 
  geom_point() + facet_wrap(~ pitch_type, ncol = 3) +
  opts(title = 'Proportion of Pitches Resulting in B/S/X vs Pitch Number')
