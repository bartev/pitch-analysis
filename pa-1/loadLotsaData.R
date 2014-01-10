# Load some data sets

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

pitchesToKeep <- c('FF', 'SL', 'CH', 'CU', 'FA', 'FT')

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
