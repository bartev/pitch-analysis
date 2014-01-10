# Look at spin angle vs pitch type
# Adjust spin angle as in examineSpin.R

mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'


# Load all data
setwd(mainDir)
setwd(dataDir)

setwd(dataDir)
load('pfxData09.Rdat')    # pitchfx '09 data - full monty

library(plyr)
library(ggplot2)
library(reshape2)
library(hexbin)


df <- subset(pd, select=c(pitcher, pitch_type, px, pz, pfx_x, pfx_z, 
                          spin_dir, spin_rate, start_speed, end_speed))

# Convert angles s.t. 0 degrees is horizontal, 90 is straight up, 180 horiz
# and negative degrees indicate back spin
Sin <- function(theta) sin(theta * pi / 180)
Cos <- function(theta) cos(theta * pi / 180)

df <- mutate(df, 
             SpinAngle = (spin_dir + 270) %% 360 - 180,
             VertSpin = Sin(SpinAngle) * spin_rate,
             HorizSpin = Cos(SpinAngle) * spin_rate
             )
df <- rename(df, c(px = 'xPos', pz = 'zPos', pfx_x = 'xSpinDefl', pfx_z = 'zSpinDefl', spin_rate = 'SpinRate'))

# count(df, vars = 'pitch_type')
# pitch_type   freq
# http://www.hardballtimes.com/main/article/pitch-identification-tutorial/
# 1          AB     11
# 2          CH  83512  Changeup, same motion as FB, but slower (low 80's, 10-15mph < fast)
# 3          CU  65605  Curveball (cutter?) slowest pitch (70's) negative vert movement
# 4          EP      2
# 5          FA  30413  Fast
# 6          FC  18462
# 7          FF 345788  Four seam fast ball  (some vert action, no horiz action)
# 8          FS    441
# 9          FT  26530  Two seam fast (some vert action, no horiz action)
# 10         IN   3549
# 11         KN   2474
# 12         PO    485
# 13         SC     25
# 14         SI  11842
# 15         SL 118963  Slider? (btw fast & curve in speed& movement) (high 80's)
# 16         UN    159

# Splitter - split finger fast ball. Change up speed (low 80's)
# Sidearm pitchers usually only throw a fast & slider

# this website suggests CU = changeup, CB = curveball
# http://www.hardballtimes.com/main/article/fastball-slider-changeup-curveball-an-analysis/

# What pitches to concentrate on?
# Remove some less common pitches
df.tbl <- count(df, 'pitch_type')
df.tbl <- mutate(df.tbl, rank=rank(-freq), pct=100 * freq / sum(freq))
df.tbl <- arrange(df.tbl, rank)
df.tbl <- transform(df.tbl, pitch_type=reorder(pitch_type, -rank))

# Plot pitch type vs frequency - could change to percentage...
ptf.plot <- qplot(pitch_type, freq, data=df.tbl, geom='bar') + coord_flip()
ptitle <- opts(plot.title = theme_text(size = 14, lineheight = 1.2, face = 'bold'))
topts <- opts(title = "Pitch Type Frequency")
ptf.plot + topts + ptitle + xlab('Pitch Type') + ylab('Frequency')

# Plot of Top 6 
df.top6 <- subset(df.tbl, rank <= 6)
df.top6.plt <- ggplot(df.top6, aes(pitch_type, pct)) + geom_bar(fill='steelblue') + coord_flip()
df.top6.plt + topts + ptitle + xlab('Pitch Type') + ylab('Percentage of Pitches') + 
  stat_bin(aes(y = pct + 3, label=paste(round(pct, 1), '%')), geom='text')

pitchesToKeep <- df.top6$pitch_type
df.6 <- subset(df, pitch_type %in% pitchesToKeep)
# Get rid of unused levels
df.6$pitch_type <- factor(df.6$pitch_type)

# Who are the pitchers?
up <- unique(df.6$pitcher)

# Look at 1 pitcher
df.a <- subset(df.6, pitcher == 400061)
zeroLines <- geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
gv <- geom_vline(xintercept = 0)
gh <- geom_hline(yintercept = 0)
ggplot(df.a, aes(xPos, zPos)) + geom_hex() + facet_wrap(~ pitch_type) + gv + gh

df.lincecum <- subset(df.6, pitcher == lincecumId & pitch_type != 'FT')
ggplot(df.lincecum, aes(xPos, zPos)) + geom_hex() + facet_wrap(~ pitch_type) + gv + gh

df.top.3 <- subset(df.6, pitcher %in% id.3.pitchers)
ggplot(df.top.3, aes(xPos, zPos)) + geom_hex() + facet_grid(pitcher ~ pitch_type) + gv + gh


# USED AT PAW
df.toplot <- subset(df.6, pitcher %in% pitcher.3$pitcher & pitch_type != 'FA')
df.toplot <- join(df.toplot, pitcher.3)
ggplot(df.toplot, aes(xPos, zPos)) + geom_hex() + facet_grid(last_name ~ pitch_type) + gv + gh

# Top 10 pitchers in # of pitches
mostPitches <- count(df, vars = c('pitcher'))
mostPitches <- arrange(mostPitches, desc(freq))
df.most <- subset(df, pitcher %in% head(mostPitches$pitcher, 10))


# Remove some pitch types
pitchesToRemove <- c('AB', 'FS', 'IN', 'PO', 'SI', 'UN', 'EP', 'KN', 'SC')
df.most <- subset(df.most, !pitch_type %in% pitchesToRemove)
ggplot(df.most, aes(xPos, zPos)) + geom_hex() + 
  facet_grid(pitcher ~ pitch_type) + gv + gh

# Look at top pitcher, one pitch (FF)
df.one <- subset(df, pitcher==434378 & pitch_type == 'FF')
ggplot(df.one, aes(xPos, zPos)) + geom_hex() + 
  facet_grid(pitcher ~ pitch_type) + gv + gh

# Use melt/dcast to get mean/sd of each variable
tidy <- melt(subset(df, !pitch_type %in% pitchesToRemove), 
             c('pitcher', 'pitch_type'))
df.x.mean <- dcast(tidy, pitcher + pitch_type ~ variable, mean)
df.x.sd <- dcast(tidy, pitcher + pitch_type ~ variable, sd)

# remove some pitches
# Plot avg & sd x, y displacement for each pitcher for each pitch - color = speed
ggplot(data=df.x.mean, aes(xPos, zPos, colour = end_speed)) + 
  geom_point() + facet_wrap(~ pitch_type) + gv + gh
ggplot(data=df.x.sd, aes(xPos, zPos, colour = end_speed)) + 
  geom_point() + facet_wrap(~ pitch_type)

# Plot avg/sd speed for each pitcher for each pitch
# find order of median speed
medEndSpeed <- ddply(df.x.mean, 'pitch_type', summarise, medSpeed = median(end_speed))
medEndSpeed <- arrange(medEndSpeed, desc(medSpeed))
# reorder factors lowest to highest end_speed
df.x.mean$pitch_type <- factor(df.x.mean$pitch_type, levels = medEndSpeed$pitch_type)
# Add vlines for median end_speed sd
medEndSpeedMean <- ddply(df.x.mean, 'pitch_type', summarise, 
                       medSpeed = median(end_speed, na.rm=T))
ggplot(data=df.x.mean, aes(end_speed )) + geom_bar() + 
  geom_vline(aes(xintercept = medSpeed), data = medEndSpeedMean) +
  facet_grid(pitch_type ~ .)



# Repeat for sd
df.x.sd$pitch_type <- factor(df.x.sd$pitch_type, levels = medEndSpeed$pitch_type)
# Add vlines for median end_speed sd
medEndSpeedSd <- ddply(df.x.sd, 'pitch_type', summarise, 
                       medSpeed = median(end_speed, na.rm=T))
ggplot(data=df.x.sd, aes(end_speed )) + geom_bar() +
  geom_vline(aes(xintercept = medSpeed), data =  medEndSpeedSd) +
  facet_grid(pitch_type ~ .)

#### Look at spin deflection like we did position

## looks like there may be some LH/RH behavior here.
## Add handedness of pitcher
load(paste(dataDir,'players09.Rda', sep=""))
pitchers <- subset(pl, select=c(id, throws))
names(pitchers)[1] <- 'pitcher'
df.x.mean <- join(df.x.mean, pitchers, type = 'left')
df.x.sd <- join(df.x.sd, pitchers, type = 'left')
df.x.mean$throws <- factor(df.x.mean$throws, levels=c('R', 'L'))
df.x.sd$throws <- factor(df.x.sd$throws, levels=c('R', 'L'))

# Plot avg & sd x, y displacement for each pitcher for each pitch - color = speed
ggplot(data=df.x.mean, aes(xSpinDefl, zSpinDefl, colour = end_speed)) + 
  geom_point() + facet_grid(pitch_type ~ throws) + gv + gh +
  opts(title = 'Mean Deflection due to Spin For Each Pitcher') + ptitle

ggplot(data=df.x.sd, aes(xSpinDefl, zSpinDefl, colour = end_speed)) + 
  geom_point() + facet_grid(pitch_type ~ throws) +
  opts(title = 'SD of Deflection due to Spin For Each Pitcher') + ptitle


#### Look at SpinAngle & SpinRate like we did position
ggplot(data=df.x.mean, aes(SpinAngle, SpinRate, colour = end_speed)) + 
  geom_point() + facet_grid(pitch_type ~ throws) + gv +
  opts(title = 'Mean SpinRate & SpinAngle For Each Pitcher') + ptitle

ggplot(data=df.x.sd, aes(SpinAngle, SpinRate, colour = end_speed)) + 
  geom_point() + facet_grid(pitch_type ~ throws) +
  opts(title = 'SD SpinRate & SpinAngle For Each Pitcher') + ptitle


## Put in one DataFrame using melt
tidysd <- melt(df.x.sd, c('pitcher', 'pitch_type', 'throws'))
names(tidysd)[5] <- 'StdDev'
tidymean <- melt(df.x.mean, c('pitcher', 'pitch_type', 'throws'))
names(tidymean)[5] <- 'Mean'
tidyMeanSd <- join(tidysd, tidymean, type='full')


## Save data
save(tidymean, tidysd, tidyMeanSd, file='tidyMeanSd2.Rda')
# load('tidyMeanSd2.Rda')

# Convert back to df using dcast
# dc <- dcast(tidyMeanSd, pitcher + pitch_type + throws ~ variable, 
#             value.var = 'StdDev')

