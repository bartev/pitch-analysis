# Find ER & ERA
# ER = earned runs
# ERA = ER/(num innings pitched)

# uses data from swingStrikes.R

mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggplot2)
library(plyr)
library(reshape2)
library(sqldf)

setwd(dataDir)
load('pfxData09.Rdat')    # pitchfx '09 data - full monty
setwd(mainDir)

# How often did each pitcher pitch in each inning?
# df.pi.alternate <- count(subset(ab, select=c(pitcher, inning)))
df.pi <- count(ab, c('pitcher', 'inning'))
df.pi.wide <- dcast(df.pi, pitcher ~ inning)
# head(df.pi.wide, 3)
# pitcher  1  2  3  4  5  6   7  8  9 10 11 12 13 14 15 16 17 18
# 1  110683 NA  9  4  5 18 65 109 69 40  4 NA NA NA  3 NA NA NA NA
# 2  111492 NA  1 NA NA  4 10  19 33 18 NA NA NA NA NA NA NA NA NA
# 3  111838 29 26 29 21 21 17  10  3 NA NA NA NA NA NA NA NA NA NA

# Who are starting pitchers?
df.starters <- df.pi.wide[,1:2]
names(df.starters)[2] <- 'inningsStarted'
df.starters <- mutate(df.starters, StartRelief = ifelse(is.na(inningsStarted), 'relief', 'starter'))

# How many innings did a pitcher pitch in?
ug <- unique(subset(ab, select=c(pitcher, game_id, inning)))
df.PitcherInnings <- count(ug, 'pitcher')
names(df.PitcherInnings)[2] <- 'numInningsPitched'
# pitcher numInningsPitched
# 1  110683                84
# 2  111492                20
# 3  111838                36

# How many batters did each pitcher pitch to?
df.numBatters <- count(ab, 'pitcher')
names(df.numBatters)[2] <- 'numAtBats'
# pitcher numAtBats
# 1  110683       326
# 2  111492        85
# 3  111838       156

# Find ERA
# sqldf is faster than ddply here
# > system.time(tmp2 <- ddply(ab, 'pitcher', summarise, ER = sum(runsOnPitcher)))
# user  system elapsed 
# 2.720   1.927   4.690 
# > system.time(eraDf <- sqldf('select pitcher, sum(runsOnPitcher) ER from ab group by pitcher'))
# user  system elapsed 
# 1.827   0.764   2.648 
eraDf <- sqldf('select pitcher, sum(runsOnPitcher) ER from ab group by pitcher')

### BUILD UP PITCHER DF
pitcherDf <- join(df.starters, df.PitcherInnings, type='left')
pitcherDf <- join(pitcherDf, df.numBatters, type='left')
pitcherDf <- join(pitcherDf, eraDf, type='left')
pitcherDf <- mutate(pitcherDf, ERA = 9 * ER / numInningsPitched)

# From swingStrikes.R (slightly modified - there filter for 500+ pitches)
pd$des <- gsub('[ (),]', '', pd$des)

ft <- count(pd, c('pitcher', 'des'))
df.des <- dcast(ft, pitcher ~ des)

# How many total pitches?
ft.totPitches <- count(pd, 'pitcher')
names(ft.totPitches)[2] <- 'TotalPitches'
df.des <- join(df.des, ft.totPitches, type='left')
# Get subset of columns
df.des <- subset(df.des, select=c(pitcher, SwingingStrike, CalledStrike, TotalPitches))
# Look at percentage of TotalPitches also
df.des <- mutate(df.des, 
              pctSwingStrike = SwingingStrike / TotalPitches,
              pctCalledStrike = CalledStrike / TotalPitches,
              swingVsCalledStrike = SwingingStrike / CalledStrike
              )

pitcherDf <- join(pitcherDf, df.des)

# Take a subset to avoid looking at pitchers that don't pitch often
pitcherDf.f <- subset(pitcherDf, TotalPitches > 500)

####### SIDE NOTE #######
### How many pitches per inning on average?
m.ppi <- lm(TotalPitches ~ numInningsPitched - 1, data = pitcherDf)
# summary(m.ppi)
# 
# Call:
#   lm(formula = TotalPitches ~ numInningsPitched - 1, data = pitcherDf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -580.88  -84.86    1.19   47.07  363.71 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# numInningsPitched 14.88735    0.05599   265.9   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Residual standard error: 137.5 on 663 degrees of freedom
# Multiple R-squared: 0.9907,  Adjusted R-squared: 0.9907 
# F-statistic: 7.069e+04 on 1 and 663 DF,  p-value: < 2.2e-16 

### A plot to look at the residual of TotalPitches ~ numInningsPitched
# temp <- m.ppi$model
# temp$resid <- resid(m.ppi)
# temp$fit <- fitted(m.ppi)
ggplot(temp, aes(numInningsPitched, resid)) + geom_point() + 
  geom_point(aes(y=fit), colour='red', alpha=I(1/2)) + 
  geom_hline(yintercept = 0, colour = 'blue')

####### END SIDE NOTE #######
goodLimits <- summarise(pitcherDf.f, 
                        eraQ1 = quantile(ERA, 0.25), 
                        pssQ3 = quantile(pctSwingStrike, 0.75),
                        pcsQ3 = quantile(pctCalledStrike, 0.75))
### Plot % Swinging Strike vs ERA ###
# Top left corner is best (low ERA, high % swing strike)
ggplot(pitcherDf.f, aes(x=ERA, y=pctSwingStrike, colour = numInningsPitched)) + 
  geom_point() + geom_vline(x=goodLimits$eraQ1) + geom_hline(y=goodLimits$pssQ3)

ggplot(pitcherDf.f, aes(x=ERA, y=pctCalledStrike, colour = numInningsPitched)) + 
  geom_point() + geom_vline(x=goodLimits$eraQ1) + geom_hline(y=goodLimits$pcsQ3)

molten <- melt(pitcherDf.f, id.vars = c('pitcher', 'StartRelief', 'ERA', 'numInningsPitched'))
ggplot(subset(molten, variable %in% c('pctSwingStrike', 'pctCalledStrike')),
       aes(ERA, value, alpha = numInningsPitched, colour = variable)) +
         geom_point() +
         geom_vline(x=goodLimits$eraQ1, colour='black') + 
         geom_hline(y=goodLimits$pcsQ3, colour='blue') +
         geom_hline(y=goodLimits$pssQ3, colour='red') +
         ylab('Percent of Pitches') +
         opts(title='Swinging & Called Strikes vs. ERA')



### Save data for cluster analysis ###
setwd(dataDir)
save(pitcherDf, pitcherDf.f, file='pitcherDf.Rdat') # pitcherDf.f is filtered on TotalPitches > 500
setwd(mainDir)
