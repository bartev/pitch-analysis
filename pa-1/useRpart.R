# Use rpart


mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

library(ggdendro)
library(ggplot2)
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

# Step 1
# Limit to starting pitchers
df <- subset(pitcherDf, StartRelief == 'starter')
df <- join(df, pitcherClusterDf)
df <- df[, -3] # remove the StartRelief column

# from perPitchSummaries.R
goodDes <- c('Called Strike', 'Foul Tip', 'Foul', 'Swinging Strike', 'Swinging Strike (Blocked)', 
             'Foul (Runner Going)', 'Missed Bunt', 'Foul Bunt', 'Unknown Strike', 'In play, out(s)')
badDes <- c('In play, run(s)', 'Ball', 'Ball In Dirt', 'In play, no out', 
            'Hit By Pitch')

pd.tmp <- mutate(pd, 
                 SpinAngle = (spin_dir + 270) %% 360 - 180,
                 VertSpin = Sin(SpinAngle) * spin_rate,
                 HorizSpin = Cos(SpinAngle) * spin_rate,
                 des2 = ifelse(des %in% goodDes, 'good', 'bad'),
                 dSpeed = end_speed - start_speed,
                 y = ifelse(des2 == 'good', 1, 0)
                 )

pd.tmp <- subset(pd.tmp,
                 select=c( pitcher, batter, b, s, des2, type, start_speed, end_speed, dSpeed, pfx_x, pfx_z,
                           pitch_type, SpinAngle, VertSpin, HorizSpin, spin_rate, pxScale, pzScale, inZone,
                           count, home))
tmp <- count(pd.tmp, c('pitcher', 'pitch_type', 'des2'))
# head(tmp)
# pitcher pitch_type des2 freq
# 1  110683         CH  bad   24
# 2  110683         CH good   10
# 3  110683         CU  bad    5
# 4  110683         CU good    2
# 5  110683         FC  bad   43
# 6  110683         FC good   70

tmp <- count(pd.tmp, c('pitch_type', 'des2'))
dcast(tmp, pitch_type ~ des2)
# Using freq as value column: use value.var to override.
# pitch_type    bad   good
# 1          AB     11     NA
# 2          CH  38681  44831
# 3          CU  31015  34590
# 4          EP      2     NA
# 5          FA  13871  16542
# 6          FC   7342  11120
# 7          FF 146768 199020
# 8          FS    213    228
# 9          FT  12825  13705
# 10         IN   3549     NA
# 11         KN   1097   1377
# 12         PO    485     NA
# 13         SC     11     14
# 14         SI   5638   6204
# 15         SL  51175  67788
# 16         UN    134     25

# grow tree
# use rpart to fit des2 (good/bad outcomes of each pitch)
# results:
# pxScale and pzScale are the only real indicators to a good/bad outcome
# -0.93 < pzScale < 1.23
# -1.76 < pxScale < 1.36
fit <- rpart(des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + VertSpin + HorizSpin + spin_rate + pxScale + pzScale, 
             method = 'class', data = pd.tmp)
fit <- rpart(des2 ~ pxScale + pzScale + inZone, method = 'class', data = pd.tmp)
print(fit)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform = T, main='rpart des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + \nVertSpin + HorizSpin + spin_rate + pxScale + pzScale')
plot(fit, uniform = T, main='rpart des2 ~ pxScale + pzScale + inZone')
text(fit, use.n=T, all = T, cex = 0.8)
post(fit, file='rpartXZScaleInZone.ps')  # create ps file

# look at multiple pitch types. 
# run fit <- rpart(...) for each pitch type, use fitr <- dendro_data(fit) & ggplot to plot out tree
# change title for each tree
# USED IN PAW
fit <- rpart(des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + VertSpin + HorizSpin + spin_rate + pxScale + pzScale + inZone, 
             method = 'class', data = subset(pd.tmp, pitch_type == 'FF'))

fit <- rpart(des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + VertSpin + HorizSpin + spin_rate + pxScale + pzScale + inZone, 
             method = 'class', data = subset(pd.tmp, pitch_type == 'CU'))
fit <- rpart(des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + VertSpin + HorizSpin + spin_rate + pxScale + pzScale + inZone, 
             method = 'class', data = subset(pd.tmp, pitch_type == 'SL'))
fit <- rpart(des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + VertSpin + HorizSpin + spin_rate + pxScale + pzScale + inZone, 
             method = 'class', data = subset(pd.tmp, pitch_type == 'CH'))
fit <- rpart(des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + VertSpin + HorizSpin + spin_rate + pxScale + pzScale + inZone, 
             method = 'class', data = subset(pd.tmp, pitch_type == 'FA'))
fit <- rpart(des2 ~ pfx_x + pfx_z + end_speed + SpinAngle + VertSpin + HorizSpin + spin_rate + pxScale + pzScale + inZone, 
             method = 'class', data = subset(pd.tmp, pitch_type == 'KN'))

# examine the fit
# Again, only pxScale and pzScale are important
printcp(fit, digits = 2)
summary(fit)

# use ggdendro to plot tree

# USED IN PAW
fitr <- dendro_data(fit)
ggplot() + geom_segment(data=fitr$segments, aes(x=x, y= y, xend=xend, yend=yend)) +
  geom_text(data=fitr$labels, aes(x=x, y=y, label=label), vjust=-0.5) +
  geom_text(data=fitr$leaf_labels, aes(x=x, y=y, label=label), vjust=1) +
  theme_dendro() +
  opts(title = 'Decision Tree for Good/Bad Outcomes for FF (rpart)\n') + ptitle

opts(title = 'Decision Tree for Good/Bad Outcomes for FA (rpart)')
opts(title = 'Decision Tree for Good/Bad Outcomes for CU (rpart)')
opts(title = 'Decision Tree for Good/Bad Outcomes for SL (rpart)')
opts(title = 'Decision Tree for Good/Bad Outcomes for CH (rpart)')
opts(title = 'Decision Tree for Good/Bad Outcomes for FA (rpart)')

# VertSpin is an indicator for pitch_type == 'CH'
# look at cdplot
cdplot.dat <- subset(pd.tmp, pitch_type == 'CH')
cdplot.dat$des2 <- as.factor(cdplot.dat$des2)
cdplot(des2 ~ VertSpin, data=cdplot.dat)

cdplot.dat <- subset(pd.tmp, pitch_type == 'FA')
cdplot.dat$des2 <- as.factor(cdplot.dat$des2)
cdplot(des2 ~ pzScale, data=cdplot.dat)
