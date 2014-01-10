# pitch level features
# After running pitcherSummaries.R

# incomplete - end is messy

setwd(mainDir)
source('fnUtilityFunctions.R')


CombineEvents <- function(v) {
  # Order matters so because using partial search
  v <- gsub('Batter Interference', 'Interference', v)
  v <- gsub('Bunt Groundout', 'Out', v)
  v <- gsub('Bunt Ground Out', 'Out', v)
  v <- gsub('Bunt Pop Out', 'Out', v)
  
  v <- gsub('Double Play', 'DP', v)
  v <- gsub('Fan interference', 'Interference', v)
  v <- gsub('Field Error', 'Err', v)
  v <- gsub('Fielders Choice Out', 'Out', v)
  v <- gsub('Fielders Choice', 'Out', v)
  v <- gsub('Fly Out', 'Out', v)
  v <- gsub('Flyout', 'Out', v)
  v <- gsub('Force Out', 'Out', v)
  v <- gsub('Forceout', 'Out', v)
  v <- gsub('Ground Out', 'Out', v)
  v <- gsub('Grounded Into DP', 'DP', v)
  v <- gsub('Groundout', 'Out', v)
  v <- gsub('Home Run', 'HR', v)
  v <- gsub('Line Out', 'Out', v)
  v <- gsub('Lineout', 'Out', v)
  v <- gsub('Pop Out', 'Out', v)
  v <- gsub('Sac Bunt', 'Out', v)
  v <- gsub('Sac Fly DP', 'DP', v)
  v <- gsub('Sac Fly', 'Out', v)
  v <- gsub('Sacrifice Bunt DP', 'DP', v)
  
  v <- gsub('Triple Play', 'TP', v)
  
  # Not hits
  v <- gsub('Catcher Interference', 'Interference', v)
  v <- gsub('Hit By Pitch', 'HBP', v)
  v <- gsub('Intent Walk', 'IBB', v)
  v <- gsub('Runner Out', 'RunnerOut', v)
  v <- gsub('Strikeout - DP', 'K', v)
  v <- gsub('Strikeout', 'K', v)
}

goodDes <- c('Called Strike', 'Foul Tip', 'Foul', 'Swinging Strike', 'Swinging Strike (Blocked)', 
             'Foul (Runner Going)', 'Missed Bunt', 'Foul Bunt', 'Unknown Strike', 'In play, out(s)')
badDes <- c('In play, run(s)', 'Ball', 'Ball In Dirt', 'In play, no out', 
            'Hit By Pitch')

ab.tmp <- subset(ab, select=c(game_id, ubnum, event, stand, p_throws, half, htRuns, atRuns))
pd.tmp <- subset(pd, select=c(game_id, ubnum, pitcher, batter, des, type, pitch_type, pitchNum, finalPitch, seasonPitchNum))
pdab <- join(pd.tmp, ab.tmp)
pdab <- subset(pdab, pitch_type %in% pitchesToKeep)
# Simplify event coding (combine codes)
pdab$event <- CombineEvents(pdab$event)



# What pitches resulted in different events (look at pitch and event only)
# only keep most common pitches
pd.tmp <- subset(pd, pitch_type %in% pitchesToKeep,
                 select=c(game_id, ubnum, pitcher, batter, des, type, pitch_type, 
                          pfx_x, pfx_z, spin_dir, spin_rate, start_speed, end_speed,
                          pxScale, pzScale, inZone,
                          pitchNum, finalPitch, seasonPitchNum
                          ))
# pd.tmp$des2 <- ifelse(pd.tmp$des %in% goodDes, 'good', 'bad')
pd.tmp <- mutate(pd.tmp, 
                 SpinAngle = (spin_dir + 270) %% 360 - 180,
                 VertSpin = Sin(SpinAngle) * spin_rate,
                 HorizSpin = Cos(SpinAngle) * spin_rate,
                 des2 = ifelse(pd.tmp$des %in% goodDes, 'good', 'bad'),
                 dSpeed = end_speed - start_speed,
                 y = ifelse(des2 == 'good', 1, 0)
                 )

dftm <- pd.tmp  # df to model


fit <- glm(y ~ pitch_type:pfx_x + pitch_type:pfx_z + pitch_type:SpinAngle +
  pitch_type:VertSpin + pitch_type:HorizSpin + dSpeed, data=dftm[trainIn, ], family = binomial)

dftm <- subset(pd.tmp, pitch_type == 'FF')
dftm <- subset(dftm, select=c(y, des2, pfx_x, pfx_z, SpinAngle, spin_rate, start_speed, end_speed, dSpeed,
                              pxScale, pzScale, VertSpin, HorizSpin))
dftm$y <- as.factor(dftm$y)
dftm$des2 <- as.factor(dftm$des2)

# COOL STUFF HERE
# Conditional Density Plots
# show how categorical variable (des2) changes with a numerical variable.
cdplot(des2 ~ pfx_x, data = dftm)
cdplot(des2 ~ pfx_z, data = dftm)
cdplot(des2 ~ SpinAngle, data = dftm)
cdplot(des2 ~ spin_rate, data = dftm)
cdplot(des2 ~ start_speed, data = dftm)
cdplot(des2 ~ end_speed, data = dftm)
cdplot(des2 ~ dSpeed, data = dftm)
cdplot(des2 ~ pxScale, data = dftm)
cdplot(des2 ~ pzScale, data = dftm)
cdplot(des2 ~ VertSpin, data = dftm)
cdplot(des2 ~ HorizSpin, data = dftm)
cdplot(des2 ~ inZone, data = dftm)

# INTERESTING TIDBITS
# If FF is in strike zone, prob of good outcome =
# > 140865 / (140865 + 55942)
# [1] 0.7158
summary(subset(dftm, abs(pxScale) < 1))

summary(subset(dftm, pzScale > -.2 & pzScale < .2 ))

# Found these rules manually
summary(subset(dftm, abs(pxScale) < 1.2 ))  # B1
summary(subset(dftm, pzScale < 1.2 & pzScale > -.5))  # B2


# Prior probabilities
# Bayes
# B1 = subset(dftm, abs(pxScale) < 1.2 )
# B2 = subset(dftm, pzScale < 1.2 & pzScale > -.5)
pb.1 <- nrow(subset(dftm, abs(pxScale) < 1.2)) / nrow(dftm)
pb.2 <- nrow(subset(dftm, pzScale > -0.5 & pzScale < 1.2)) / nrow(dftm)
dftm.g <- subset(dftm, des2 == 'good')
pb.1.g <- nrow(subset(dftm.g, abs(pxScale) < 1.2)) / nrow(dftm.g)
pb.2.g <- nrow(subset(dftm.g, pzScale > -0.5 & pzScale < 1.2)) / nrow(dftm.g)
pg <-  nrow(dftm.g) / nrow(dftm)

pg.b.1 <- pb.1.g * pg / pb.1
pg.b.2 <- pb.2.g * pg / pb.2


# Scale features - didn't use?
dftm <- mutate(dftm, 
               pfx_x = pfx_x - mean(pfx_x),
               pfx_z = pfx_z - mean(pfx_z),
               SpinAngle = pi * SpinAngle / 180,
               spin_rate = spin_rate - mean(spin_rate), 
               start_speed = start_speed - mean(start_speed),
               end_speed = end_speed - mean(end_speed),
               dSpeed = dSpeed - mean(dSpeed),
               pxScale = pxScale - mean(pxScale),
               pzScale = pzScale - mean(pzScale),
               VertSpin = (VertSpin - mean(VertSpin)) / range(VertSpin),
               HorizSpin = (HorizSpin - mean(HorizSpin)) / range(HorizSpin)
               )
# dftm <- mutate(pfx_x.S = scale(pfx_x))


trainIn <- sample(1:nrow(dftm), 0.6 * nrow(dftm), replace = FALSE)
testIn <- which(! 1:nrow(dftm) %in% trainIn)
fit <- glm(y ~ pfx_x + pfx_z + SpinAngle + spin_rate + start_speed + end_speed + dSpeed +
  pxScale + pzScale + VertSpin + HorizSpin + inZone, data=dftm[trainIn, ], family = binomial)

fit <- glm(y ~ spin_rate + pxScale + pzScale + inZone, data=dftm[trainIn, ], family = binomial)

GetLift <- function(df, model){
  newdf <- cbind(df, predLogit=predict(model, df))
  newdf <- transform(newdf, yPred=predLogit >= 0)
  confusionMatrix <- with(newdf, xtabs(~ best + yPred))
  result <- prec(confusionMatrix)
}

myGetLift <- function(df, model) {
  newdf <- mutate(df,
                  predLogit = predict(model, df),
                  yPred = predLogit >= 0.5
                  )
  cm <- with(newdf, xtabs(~ y + yPred))
  newdf  
}
# cm <- myGetLift(dftm[trainIn, ], fit)
fit.df <- myGetLift(dftm[trainIn, ], fit)
cm <- with(fit.df, xtabs(~ y + yPred))
p <- prec(cm); p

count(fit.df, c('y', 'yPred'))

head(fit.df)
ggplot(fit.df, aes(yPred, predLogit)) + geom_boxplot() + facet_wrap(~ y)
ggplot(fit.df, aes(seasonPitchNum, predLogit)) + geom_boxplot() + facet_wrap(~ y)

# > attributes(fit)
# $names
# [1] "coefficients"      "residuals"         "fitted.values"     "effects"           "R"                
# [6] "rank"              "qr"                "family"            "linear.predictors" "deviance"         
# [11] "aic"               "null.deviance"     "iter"              "weights"           "prior.weights"    
# [16] "df.residual"       "df.null"           "y"                 "converged"         "boundary"         
# [21] "model"             "call"              "formula"           "terms"             "data"             
# [26] "offset"            "control"           "method"            "contrasts"         "xlevels"          

# stop here








### INCOMPLETE

ab.tmp <- subset(ab, select=c(game_id, ubnum, pitcher, batter, stand, event, half))
ab.tmp$event <- CombineEvents(ab.tmp$event)

# pdab.x <- subset(pdab, type == 'X')
# pdab.b <- subset(pdab, type == 'B')
# pdab.s <- subset(pdab, type == 'S')

cntAll <- count(pdab, c('type', 'event', 'pitch_type', 'finalPitch'))
# Good table here, shows event vs. pitch type


cntEvent <- count(pdab.x, c('pitch_type', 'event'))
dcast(cntEvent, event ~ pitch_type)


# Here look at prop table of same thing
cntEvent.prop <- ddply(cntEvent, 'event', mutate, propFreq = freq / sum(freq))
options(digits = 2)
dcast(cntEvent.prop, event ~ pitch_type, value.var= 'propFreq')


df1.c <- count(df1, c('type', 'pitch_type'))
test <- ddply(df1.c, 'pitch_type', mutate, propFreq = freq / sum(freq))
dcast(test, pitch_type ~ type)
