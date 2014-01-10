# When are runs scored during a game?
mainDir <- '~/Rmac/Pitchfx/'
setwd(mainDir)
source('loadLotsaData.R')

whenRunsOnPitcher <- ddply(ab, 'inning', 
                           function(df) summarise(df, AvgRunsOnPitcher = 
                             sum(runsOnPitcher) / nrow(unique(subset(df, select=c(game_id, half))))))
whenRunsOnPitcher <- subset(whenRunsOnPitcher, inning < 10)

# USED IN PAW
ggplot(whenRunsOnPitcher, aes(inning, AvgRunsOnPitcher)) + geom_smooth(fill='red', alpha=0.2) +
  geom_line() + geom_point(size = 3) +
  opts(title='Average Number of Runs Scored Per Team Per Inning') + ptitle +
  xlab('Inning Number') + ylab('Runs In An Inning')


psi <- subset(pd, select=c(pitcher, game_id, ubnum, end_speed, type, pitch_type, seasonPitchNum,
                           pfx_x, pfx_z, pxScale, pzScale))
psi.tmp <- subset(ab, select=c(ubnum, inning, half, runsOnPitcher))
psi.tmp$ubnum <- as.factor(psi.tmp$ubnum)
psi <- join(psi, psi.tmp)
psi$pitcher <- as.factor(psi$pitcher)

### Add Start/Relief

startRelief <- subset(pitcherDf, select=c(pitcher, StartRelief))
psi <- join(psi, startRelief)
psi$StartRelief <- factor(psi$StartRelief, levels = c('starter', 'relief'))
# pitchesToKeep
# [1] "FF" "SL" "CH" "CU" "FA" "FT"
psi <- subset(psi, pitch_type %in% pitchesToKeep & inning < 10)

speedDuringGame <- ddply(psi, c('inning', 'pitch_type', 'StartRelief'), function(df) summarise(df, speed = mean(end_speed)))

# USED IN PAW
ggplot(speedDuringGame, aes(inning, speed, colour=StartRelief)) + geom_point() + geom_line() + 
  facet_wrap(~ pitch_type) +
  opts(title = 'Average Pitch Speed Per Inning') + ptitle + 
  xlab('Inning Number') + ylab('Speed (mph)')

spinEffectDuringGame <- ddply(psi, c('inning', 'pitch_type', 'StartRelief'),
                              function(df) summarise(df,
                                                     VertDisp = mean(pfx_z), HorizDisp = mean(pfx_x),
                                                     sdVertDisp = sd(pfx_z), sdHorizDisp = sd(pfx_x),
                                                     xPos = mean(pxScale), zPos = mean(pzScale),
                                                     sdXPos = sd(pxScale), sdZPos = sd(pzScale)))

ggplot(spinEffectDuringGame, aes(inning, VertDisp, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)
ggplot(spinEffectDuringGame, aes(inning, sdVertDisp, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)
ggplot(spinEffectDuringGame, aes(inning, HorizDisp, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)
ggplot(spinEffectDuringGame, aes(inning, sdHorizDisp, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)
ggplot(spinEffectDuringGame, aes(inning, xPos, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)
ggplot(spinEffectDuringGame, aes(inning, sdXPos, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)
ggplot(spinEffectDuringGame, aes(inning, zPos, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)
ggplot(spinEffectDuringGame, aes(inning, sdZPos, colour=StartRelief)) + geom_point() + geom_line() + facet_wrap(~pitch_type)


# Get averages for pitch number during a game for each pitcher
speedDuringGame <- ddply(psi, c('inning', 'pitch_type', 'StartRelief'), 
                         function(df) summarise(df, speed = mean(end_speed)))
# speed in game
sig <- subset(pd, select=c(pitcher, game_id, ubnum, end_speed, type, pitch_type, seasonPitchNum,
                           pfx_x, pfx_z, pxScale, pzScale))
sig$pitcher <- as.factor(sig$pitcher)
sig <- join(sig, startRelief)
# this crashed, not sure why
# sig <- ddply(sig, c('pitcher', 'game_id'), function(x) x$pitcherPitchNumGame = sequence(nrow(x)))

# might be faster?
# Assign a pitch number per game for each pitcher
sig <- arrange(sig, pitcher, game_id)
zz <- rle(as.character(sig$game_id))$lengths
sig$pitcherPitchNumGame <- sequence(zz)  # create sequences from 1:length for each batter
sig <- arrange(sig, seasonPitchNum)

# speedDuringGame <- ddply(psi, c('inning', 'pitch_type', 'StartRelief'), 
#                          function(df) summarise(df, speed = mean(end_speed)))

sig <- subset(sig, pitch_type %in% pitchesToKeep)
sig.speed <- ddply(sig, c('pitcherPitchNumGame', 'pitch_type', 'StartRelief'), 
                   function(df) summarise(df, speed = mean(end_speed)))

# Frequency of pitcherPitchNumGame (how many pitches as pitch # 1, 2, etc.)
pitchNumCnt <- count(sig, c('StartRelief', 'pitcherPitchNumGame'))


# Visually find cutoffs from previous plot
## 90% of values in 1st 25 pitches for relief
df <- data.frame(StartRelief=c('starter', 'relief'), cutoff=(c(100, 25)))

# USED IN PAW
ggplot(pitchNumCnt, aes(pitcherPitchNumGame, freq, colour = StartRelief)) + 
  geom_point() + geom_line() +
  xlab('Pitch Number for Pitcher During Game') + ylab('Frequency') +
  geom_vline(data=df, aes(xintercept = cutoff, colour = StartRelief, alpha = 0.9)) +
  opts(title = 'Frequency of Pitch Number In Game') + ptitle


# Average Speed vs Pitch Number in Game, starters vs relievers
# USED IN PAW
ggplot(sig.speed, aes(pitcherPitchNumGame, speed, colour=StartRelief)) +  geom_point(alpha = 0.3) +
  facet_grid(StartRelief ~ pitch_type) + geom_smooth() + geom_vline(data=df, aes(xintercept = cutoff, alpha=0.4)) +
  opts(title = 'Average Pitch Speed vs. Pitch Number In Game') + xlab('Pitch Number') + ylab('Speed (mph)') +
  labs(colour = '') +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0)) + ptitle


# Merge runs with sig
toMerge <- subset(ab, select=c(ubnum, runsOnPitcher))
toMerge$ubnum <- as.factor(toMerge$ubnum)
sig <- join(sig, toMerge)


tmp <- count(sig, c('StartRelief', 'pitch_type', 'pitcherPitchNumGame', 'runsOnPitcher'))

ggplot(tmp, aes(pitcherPitchNumGame, freq, colour = runsOnPitcher)) + 
  geom_point() + facet_grid(StartRelief ~ pitch_type)


tmp2 <- ddply(tmp, c('StartRelief', 'pitcherPitchNumGame'), function(x) 
  summarise(x, sumROP = sum(runsOnPitcher), freq = nrow(x)))
tmp2 <- ddply(tmp2, 'StartRelief', function(x) mutate(x, dens = sumROP / freq))
ggplot(tmp2, aes(pitcherPitchNumGame, dens, colour = StartRelief)) + geom_point()

