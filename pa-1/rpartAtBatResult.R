# Use rpart on final pitches only

# Build up a per pitcher db to use?

mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggdendro)
library(ggplot2)
library(plyr)
library(MASS)
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




EventWhereHit <- function(v) {
  # Order matters so because using partial search
  v <- gsub('Batter Interference', 'unknown', v)
  v <- gsub('Bunt Groundout', 'infield', v)
  v <- gsub('Bunt Ground Out', 'infield', v)
  v <- gsub('Bunt Pop Out', 'pop', v)
  v <- gsub('Double Play', 'DP', v)
#   v <- gsub('Double', 'D', v)
  v <- gsub('Fan interference', 'unknown', v)
  v <- gsub('Field Error', 'unknown', v)
  v <- gsub('Fielders Choice Out', 'unknown', v)
  v <- gsub('Fielders Choice', 'unknown', v)
  v <- gsub('Fly Out', 'fly', v)
  v <- gsub('Flyout', 'fly', v)
  v <- gsub('Force Out', 'infield', v)
  v <- gsub('Forceout', 'infield', v)
  v <- gsub('Ground Out', 'infield', v)
  v <- gsub('Grounded Into DP', 'infield', v)
  v <- gsub('Groundout', 'infield', v)
  v <- gsub('Home Run', 'HR', v)
  v <- gsub('Line Out', 'outfield', v)
  v <- gsub('Lineout', 'outfield', v)
  v <- gsub('Pop Out', 'pop', v)
  v <- gsub('Sac Bunt', 'infield', v)
  v <- gsub('Sac Fly DP', 'fly', v)
  v <- gsub('Sac Fly', 'fly', v)
  v <- gsub('Sacrifice Bunt DP', 'infield', v)
  
  v <- gsub('Triple Play', 'TP', v)
  
  # Not hits
  v <- gsub('Catcher Interference', 'unknown', v)
  v <- gsub('Hit By Pitch', 'unknown', v)
  v <- gsub('Intent Walk', 'IBB', v)
  v <- gsub('Runner Out', 'unknown', v)
  v <- gsub('Strikeout - DP', 'K', v)
  v <- gsub('Strikeout', 'K', v)
}

ab.tmp <- subset(ab, select=c(game_id, ubnum, pitcher, p_throws, batter, stand, event, runsOnPitcher))
ab.tmp$ev2 <- EventWhereHit(ab.tmp$event)
sort(unique(ab.tmp$ev2))

fit <- lm(runsOnPitcher ~ ev2 - 1, 
           data = subset(ab.tmp, ev2 %in% 
             c('Single', 'Double', 'Triple', 'HR', 'Walk', 'K', 'fly', 'infield', 'outfield', 'pop')))
summary(fit)

tmp <- ddply(ab.tmp, 'pitcher', function(df) summarise(df, ER = sum(runsOnPitcher)))
tmp2 <- ddply(ab.tmp, 'pitcher', function(df) count(df, 'ev2'))
# dtmp2[is.na(dtmp2)] <- 0  # REPLACE NA'S WITH 0'S
dtmp2 <- dcast(tmp2, pitcher ~ ev2, fill = 0)  # fill = 0 replaces missing values with 0 instead of NA
dtmp2 <- join(tmp, dtmp2)


# EXCELLENT FIT
# We can model ER based on the result of an at bat
trainIndices <- sort(sample(1:nrow(dtmp2), 0.6 * nrow(dtmp2), replace = FALSE))
testIndices <- which(! 1:nrow(dtmp2) %in% trainIndices)

fit <- lm(ER ~ Single + Double + Triple + HR + Walk + K, data = dtmp2[trainIndices, ])
df <- dtmp2[trainIndices, ]
fit2 <- lm(ER ~ Single + Double + Triple + HR + Walk + K, data = df)
fit.iof <- lm(ER ~ infield + fly, data = dtmp2[trainIndices, ])
# only keep 2 features - infield + fly
step <- stepAIC(fit.iof, direction = 'both')
step$anova

summary(fit)
summary(fit.iof)

# Plot out fits
df <- dtmp2
df$fit <- predict(fit, df)
df <- mutate(df, resid = ER - fit)
df$Test <- 'test'
df[trainIndices, 'Test'] <- 'train'
ggplot(df, aes(ER, fit, colour=Test)) + geom_point() + geom_abline(slope = 1) + facet_wrap(~ Test) +
  opts(title = 'lm(ER ~ Single + Double + Triple + HR + Walk + K), R^2 = 0.95') + ptitle +
  labs(colour = 'Data Set') +
  labs(x = 'ER (Earned Runs)', y = 'Fit of Model')

df$fit.iof <- predict(fit.iof, df)
df <- mutate(df, resid.iof = ER - fit.iof)
ggplot(df, aes(ER, fit.iof, colour=Test)) + geom_point() + geom_abline(slope = 1) + facet_wrap(~ Test) +
  opts(title = 'lm(ER ~ infield + fly)')


# Is there a correlation between x, z deflection and Single, Double, Triple, ...?
moltenBig <- rbind(moltenDisp, moltenSpin, moltenSpinAngle)
# SPLIT
dbig.ch.r <- dcast(moltenBig, pitcher ~  variable, sum, subset = .(pitch_type == 'CH' & throws == 'R'))
inningStats <- subset(pitcherDf, select=c(pitcher, StartRelief, numInningsPitched, numAtBats, ER, ERA))
fit.ch.r <- lm()

# Get numInningsPitched (also in pitcherDf)
ug <- unique(subset(ab, select=c(pitcher, game_id, inning)))
ug.nip <- count(ug, 'pitcher')

# Create list of pitcher, at bat outcome, freq & pct(freq)
cnt.event <- count(ab.tmp, c('pitcher', 'ev2'))
ce <- ddply(cnt.event, 'pitcher', summarise, atBatOutcome = ev2, freq = freq, pct = freq / sum(freq))


dcnt.event <- dcast(cnt.event, pitcher ~ ev2, fill = 0)
cnt.event.tot <- ddply(cnt.event, 'pitcher', function(df) sum(df$freq))
# 1 pitcher
ce <- subset(cnt.event, pitcher == 110683)
cem <- mutate(ce, pct = freq / sum(freq))
j <- ddply(ce, 'pitcher', summarise, pct=sum(pct), freq=sum(freq))



# What are the possible outcomes of an at bat, and their percentages?
cnt.ev2 <- count(ab.tmp, c('ev2'))
cnt.ev2 <- mutate(cnt.ev2, pctOutcome = freq / sum(freq))
# add the coefficients of fit as another column to see what the relative strength of each is
df.coeffit <- data.frame(ev2 = as.character(names(coef(fit))), coef = coef(fit))
df.coeffit <- join(cnt.ev2, df.coeffit, type='right')
df.coeffit <- mutate(df.coeffit, relWeight = pctOutcome * coef)
df.coeffit <- arrange(df.coeffit, -abs(relWeight))
df.coeffit

# QUESTION: WHAT DRIVES SINGLES VS HOME RUNS?
# Join pitch data with at bat results.
# look at final pitch only
# see what pitches resulted in what outcomes (HITS ONLY)
pitchAb <- subset(pd, type == 'X',
                  select=c(game_id, ubnum, pitcher, batter, des, type, start_speed, end_speed,
                           pfx_x, pfx_z, x0, z0, vx0, vy0, vz0, ax, ay, az, pitch_type,
                           seasonPitchNum, szBot, szTop, pxScale, pzScale, sZone, inZone,
                           pitchNum, upid, home))
ab.tmp$ubnum <- as.factor(ab.tmp$ubnum)
pitchAb$pitcher <- as.factor(pitchAb$pitcher)
pitchAb <- join(pitchAb, ab.tmp)

# Make a table of outcome ~ pitch_type
cnt.pitout <- count(pitchAb, c('pitch_type', 'ev2'))

# only keep some pitches
pitchesToKeep <- c('FF', 'SL', 'CH', 'CU', 'FA', 'FT')
eventsToDrop <- c('TP', 'IBB', 'unknown')
cnt.pitout <- subset(cnt.pitout, pitch_type %in% pitchesToKeep)
cnt.pitout <- subset(cnt.pitout, ! ev2 %in% eventsToDrop)


dcast(cnt.pitout, ev2 ~ pitch_type)
# Include margins (row and column)
dcast(cnt.pitout, ev2 ~ pitch_type, fun.aggregate = sum, margins = TRUE)

# for each pitch_type, what percent result in a given outcome?
# P(outcome | pitch_type)
pct.pitchType.pitout <- ddply(cnt.pitout, 'pitch_type', function(df) mutate(df, pct = freq / sum(freq)))
names(pct.pitchType.pitout)[4] <- 'POut_PitchType'  # P(Output | PitchType)
# dcast(pct.pitchType.pitout, pitch_type ~ ev2, fun.aggregate = sum, margins=T)

# for each outcome, what percentage come from a given pitch_type?
# P(pitch_type | outcome)
pct.outcome.pitout <- ddply(cnt.pitout, 'ev2', function(df) mutate(df, pct = freq / sum(freq)))
names(pct.outcome.pitout)[4] <- 'PPitchType_Outcome'  # P(PitchType | Output)
# dcast(pct.outcome.pitout, pitch_type ~ ev2, fun.aggregate = sum, margins=T)

# Create data.frame for Hit Outcomes
pct.cnt.pitout <- join(pct.pitchType.pitout, pct.outcome.pitout)

# pct.outcome.pitout.mean <- ddply(cnt.pitout, 'ev2', mean(pct))


### PLOTS (multi color bar charts) ####
# Plot pct ~ outcome bar plot, grouped by pitch_type
# evLev <- c('Single', 'Double', 'Triple', 'HR', 'infield', 'outfield', 'pop', 'fly', 'DP')
# pct.cnt.pitout$ev2 <- as.factor(pct.cnt.pitout$ev2)
# levels(pct.cnt.pitout$ev2) <- evLev

### P( Pitch Type | Outcome ) ### Grouped by Pitch Type
# Add means to the plot
# Change order of levels
pct.cnt.pitout <- subset(pct.cnt.pitout, ev2 != 'DP')
pct.cnt.pitout$ev2 <- factor(pct.cnt.pitout$ev2, 
                             levels = c('Single', 'Double', 'Triple', 'HR', 'infield', 'fly', 'pop', 'outfield'))

means <- ddply(pct.cnt.pitout, 'pitch_type', function(df) mean(df$PPitchType_Outcome))

# USED IN PAW
ggplot(pct.cnt.pitout, aes(x=ev2, y=PPitchType_Outcome, fill=ev2)) + geom_bar() + facet_wrap(~ pitch_type) +
  geom_hline(data=means, aes(yintercept=V1)) +
  xlab('Result of Hit') + ylab('P(Pitch Type | Result of Hit)') +
  opts(title='Probability of Pitch Type for Each Hit Outcome') + ptitle +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0)) +
  scale_fill_discrete('Result of Hit')


# Same plot, ### Grouped by Outcome instead of Pitch Type
# USED IN PAW
ggplot(pct.cnt.pitout, aes(x=pitch_type, y=PPitchType_Outcome, fill=ev2)) + geom_bar() + facet_wrap(~ ev2, nrow=2) +
  xlab('Pitch Type') + ylab('P(Pitch Type | Result of Hit)') +
  opts(title='Probability of Pitch Type for Each Hit Outcome') + ptitle +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0)) +
  scale_fill_discrete('Result of Hit')

### P (Result of Hit | Pitch Type)
### Group by Pitch Type

# USED IN PAW
ggplot(pct.cnt.pitout, aes(x=ev2, y=POut_PitchType, fill=pitch_type)) + geom_bar() + facet_wrap(~ pitch_type) +
  opts(axis.text.x=theme_text(angle= 90, hjust = 0)) +
  xlab('Result of Hit') + ylab('P (Result of Hit | Pitch Type)') +
  opts(title='Probability of Outcome For Each Pitch Type') + ptitle +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0)) +
  scale_fill_discrete('Pitch Type')

# USED IN PAW
means <- ddply(pct.cnt.pitout, 'ev2', function(df) mean(df$POut_PitchType))
ggplot(pct.cnt.pitout, aes(x=pitch_type, y=POut_PitchType, fill=pitch_type)) + geom_bar() + facet_wrap(~ ev2, nrow=2) +
  opts(axis.text.x=theme_text(angle= 90, hjust = 0)) +
  geom_hline(data=means, aes(yintercept=V1), alpha=0.5) +
  xlab('Pitch Type') + ylab('P (Result of Hit | Pitch Type)') +
  opts(title='Probability of Outcome For Each Pitch Type') + ptitle +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0)) +
  scale_fill_discrete('Pitch Type')



##### ALL PITCHES, type = B/S/X
# Look at outcomes of all pitches, not just final pitch
pitchAb.other <- subset(pd, select=c(pitch_type, des, type))

cntBS <- count(pitchAb.other, c('pitch_type', 'type'))
names(cntBS)[2] <- 'PitchOutcome'
# only keep some pitches
cntBS <- subset(cntBS, pitch_type %in% pitchesToKeep)
# Get percentages
pct.cntBS.pitch <- ddply(cntBS, 'pitch_type', function(df) mutate(df, pctPitchType = freq / sum(freq)))
pct.cntBS.outcome<- ddply(cntBS, 'PitchOutcome', function(df) mutate(df, pctOutcome = freq / sum(freq)))
pct.cntBS <- join(pct.cntBS.pitch, pct.cntBS.outcome)

mpct.cntBS.outcome <- ddply(pct.cntBS, 'PitchOutcome', function(df) mean(df$pctPitchType))
mpct.cntBS.PitchType <- ddply(pct.cntBS, 'pitch_type', function(df) mean(df$pctPitchType))
names(mpct.cntBS.outcome)[2] <- 'Mean'
names(mpct.cntBS.PitchType)[2] <- 'Mean'

# Group by Pitch Type, show Outcomes
ggplot(pct.cntBS, aes(x=PitchOutcome, y=pctPitchType, fill=pitch_type)) + geom_bar() + facet_wrap(~ pitch_type) +
  geom_hline(data=mpct.cntBS.PitchType, aes(yintercept=Mean), alpha = 0.8) +
  xlab('Ball/Strike/Hit') + ylab('P( B/S/X | Pitch Type)') +
  opts(title='Probability of B/S/X For Each Pitch Type') +
  scale_fill_discrete('Pitch Type')

# Group by Outcomes, show Outcome
ggplot(pct.cntBS, aes(x=pitch_type, y=pctPitchType, fill=pitch_type)) + geom_bar() + facet_wrap(~ PitchOutcome) +
  geom_hline(data=mpct.cntBS.outcome, aes(yintercept=Mean), alpha = 0.8) +
  xlab('Pitch Type') + ylab('P( B/S/X | Pitch Type)') +
  opts(title='Probability of B/S/X For Each Pitch Type') +
  scale_fill_discrete('Pitch Type')


### Plot pctOutcome (prob of pitch_type given outcome)
ggplot(pct.cntBS, aes(x=pitch_type, y=pctOutcome, fill=pitch_type)) + geom_bar() + facet_wrap(~ PitchOutcome) +
  ylab('P( Pitch Type | Outcome )') + xlab('Outcome') +
  opts(title = 'Probability of Pitch Type for Each Outcome (B/S/X)') +
  scale_fill_discrete('Pitch Type')

## Averages
means <- ddply(pct.cntBS, 'pitch_type', function(df) mean(df$pctOutcome))
ggplot(pct.cntBS, aes(x=PitchOutcome, y=pctOutcome, fill=pitch_type)) + geom_bar() + facet_wrap(~ pitch_type) +
  ylab('P( Pitch Type | Outcome )') + xlab('Outcome') +
  geom_hline(data=means, aes(yintercept=V1), alpha=0.8) +
  opts(title = 'Probability of Pitch Type for Each Outcome (B/S/X)') +
  scale_fill_discrete('Pitch Type')
