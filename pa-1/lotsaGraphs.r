# Combine sgls, c.ss (avgPitchCharacteristics.R) with cluster info (pctSwingERA.N.Clust.Rda)
# 

sgls.probs <- join(sgls, c.ss)
pac <- join(sgls.probs, pctSwingERA.N.Clust) # pitch and cluster

pac <- subset(pac, pitch_type %in% pitchesToKeep)
pac <- na.omit(pac)
pac$k <- as.factor(pac$k)
pac$event <- factor(pac$event, levels = c('Single', 'Double', 'Triple', 'Home Run'))

pac.r <- subset(pac, throws == 'R')

# From http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)/#a-colorblind-friendly-palette
cbgColourPalette <- scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
cbgCp <- scale_colour_manual(values=c("#33FFCC", "#990000", "#CC0033", "#33CCCC", "#336666"))
scb <- scale_colour_brewer(palette='Set1')

# Start plotting with colour = k to see what stands out
# Changed colour palette to provide more distinct colors
ggplot(pac, aes(pctSwingStrike, ERA, colour = k)) + geom_point() + 
  facet_grid(pitch_type ~ throws) + scb
tit <- opts(title='k = 2 (Blue) & 3 (Green) Have \nLoweset ERA & Highest Swinging Strikes')

ggplot(pac.r, aes(pctSwingStrike, ERA, colour = k)) + geom_point( alpha=0.5) + facet_grid(pitch_type ~ event) + scb + tit
ggplot(pac.r, aes(HorizDisp, VertDisp, colour = k)) + geom_point( alpha=0.5) + facet_grid(pitch_type ~ event) + scb + tit
toPlot <- subset(pac, pitcher %in% pitchersToKeep)
toPlot <- join(toPlot, bestAndWorst.wd)

### Plot top/bottom 10 pitchers by ERA & Swinging Strikes
ggplot(toPlot, aes(HorizDisp, VertDisp, colour = pssRank)) + 
  geom_point(alpha=0.8) + facet_grid(pitch_type ~ event) + tit
ggplot(toPlot, aes(pxScale, pzScale, colour = pssRank)) + 
  geom_point(alpha=0.8) + facet_grid(pitch_type ~ event) + tit



ggplot(pac.r, aes(vx0, vz0, colour = k)) + geom_point( alpha=0.5) + facet_grid(pitch_type ~ event) + scb + tit
ggplot(pac.r, aes(HorizSpin, VertSpin, colour = k)) + geom_point( alpha=0.5) + facet_grid(pitch_type ~ event) + scb + tit
ggplot(pac.r, aes(pxScale, pzScale, colour = k)) + geom_point( alpha=0.5) + facet_grid(pitch_type ~ event) + scb + tit

ggplot(pac.r, aes(szBot, szTop, colour = k)) + geom_point( alpha=0.5) + facet_grid(pitch_type ~ event) + scb + tit
ggplot(pac.r, aes(PEvent, PPitchType, colour = k)) + geom_point( alpha=0.5) + facet_grid(pitch_type ~ event) + scb + tit


rot <- opts(axis.text.x=theme_text(angle = 90, hjust = 0))
pac.r.s <- subset(pac.r, event == 'Single')
ggplot(pac.r.s, aes(on_1b, fill=k)) + geom_bar() + facet_grid(pitch_type ~ k) + scb + tit
ggplot(pac.r.s, aes(end_speed, fill=k)) + geom_bar() + facet_grid(pitch_type ~ k) + scb + tit
ggplot(pac.r.s, aes(ax, fill=k)) + geom_bar() + facet_grid(pitch_type ~ k) + scb + tit
ggplot(pac.r.s, aes(spin_rate, fill=k)) + geom_bar() + facet_grid(pitch_type ~ k) + scb + tit + rot
ggplot(pac.r.s, aes(pctSwingStrike, fill=k)) + geom_bar() + facet_grid(k ~ pitch_type) + scb + tit + rot
ggplot(pac.r.s, aes(HorizDisp, fill=k)) + geom_bar() + facet_grid(k ~ pitch_type) + scb + tit + rot
ggplot(pac.r.s, aes(VertDisp, fill=k)) + geom_bar() + facet_grid(k ~ pitch_type) + scb + tit + rot
ggplot(pac.r.s, aes(pxScale, fill=k)) + geom_bar() + facet_grid(k ~ pitch_type) + scb + tit + rot
ggplot(pac.r.s, aes(pzScale, fill=k)) + geom_bar() + facet_grid(k ~ pitch_type) + scb + tit + rot
ggplot(pac.r.s, aes(PEvent, fill=k)) + geom_bar() + facet_grid(k ~ pitch_type) + scb + tit + rot
ggplot(pac.r.s, aes(PPitchType, fill=k)) + geom_bar() + facet_grid(k ~ pitch_type) + scb + tit + rot


ggplot(subset(pac.r, pitch_type == 'FF'), aes(pctSwingStrike, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(HorizDisp, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
meds <- ddply(pac.r, 'event', 
              function(df) summarise(df, medH=median(HorizDisp), avgH=mean(HorizDisp), 
                                     medV=median(VertDisp), avgV=mean(VertDisp)))
qH <- ddply(pac.r, c('pitch_type', 'event'), function(df) quantile(df$HorizDisp, c(0, 0.25, 0.5, 0.75, 1.0)))
names(qH)[3:7] <- c('Q0', 'Q1', 'Q2', 'Q3', 'Q4')
qV <- ddply(pac.r,  c('pitch_type', 'event'), function(df) quantile(df$VertDisp, c(0, 0.25, 0.5, 0.75, 1.0)))
names(qV)[3:7] <- c('Q0', 'Q1', 'Q2', 'Q3', 'Q4')


### INTERESTING
# Change order of levels
# USED IN PAW
ggplot(subset(pac.r, pitch_type == 'FF'), aes(HorizDisp, ..density.., fill = event)) + geom_density(alpha = 0.3) + 
  geom_vline(data=subset(qH, pitch_type == 'FF'), aes(xintercept = c(Q2))) + 
  facet_wrap(~ event, ncol = 1) + scb  + rot +
  opts(title = 'Four Seamer - Horizontal Spin Displacement') + ptitle +
  xlab('Horizonatal Displacement (inches)') + ylab('Density')
# USED IN PAW
ggplot(subset(pac.r, pitch_type == 'FF'), aes(VertDisp, ..density.., fill = event)) + geom_density(alpha = 0.3) + 
  geom_vline(data=subset(qV, pitch_type == 'FF'), aes(xintercept = c(Q2))) + 
  facet_wrap(~ event, ncol = 1) + scb  + rot +
  opts(title = 'Four Seamer - Vertical Spin Displacement') + ptitle +
  xlab('Vertical Displacement (inches)') + ylab('Density')



ggplot(subset(pac.r, pitch_type == 'FF'), aes(VertDisp, ..density.., fill = event)) + geom_density(alpha = 0.3) + 
  geom_vline(data=meds, aes(xintercept = medV)) + facet_wrap(~ event, ncol = 1) + scb + tit + rot
ggplot(meds, aes(avgH, avgV, colour = event)) + geom_point(size = 6)
ggplot(meds, aes(medH, medV, colour = event)) + geom_point(size = 6)


ggplot(subset(pac.r, pitch_type == 'FF'), aes(pxScale, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(pzScale, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(SpinAngle, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(VertSpin, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(HorizSpin, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(pctSwingStrike, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(PEvent, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(PPitchType, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot
ggplot(subset(pac.r, pitch_type == 'FF'), aes(HorizDisp, ..density.., fill=k)) + geom_bar() + geom_density() + facet_grid(k ~ event) + scb + tit + rot

### hex plots
meds <- ddply(pac.r, c('pitch_type', 'event'),
              function(df) summarise(df, medPx = median(pxScale), medPz = median(pzScale),
                                     medHD = median(HorizDisp), medVD = median(VertDisp)))

ggplot(pac.r, aes(pxScale, pzScale)) + geom_hex() + facet_grid(pitch_type ~ event) +
  geom_vline(xintercept = 0, alpha = 0.5) + geom_hline(yintercept = 0, alpha = 0.5)
ggplot(pac.r, aes(pxScale, pzScale)) + geom_hex() + facet_grid(pitch_type ~ event) +
  geom_vline(data = meds, aes(xintercept = medPx, alpha = 0.5)) + 
  geom_hline(data = meds, aes(yintercept = medPz, alpha = 0.5))

### Interesting plot showing x,z position in strike zone 
ggplot(pac.r, aes(pxScale, pzScale)) + geom_hex() + facet_grid(pitch_type ~ event) +
  geom_vline(data = meds, aes(xintercept = medPx, alpha = 0.5)) + 
  geom_hline(data = meds, aes(yintercept = medPz, alpha = 0.5))

ggplot(pac.r, aes(HorizDisp, VertDisp)) + geom_hex() + facet_grid(pitch_type ~ event) +
  geom_vline(data = meds, aes(xintercept = medHD, alpha = 0.5)) + 
  geom_hline(data = meds, aes(yintercept = medVD, alpha = 0.5))


#### create plots
getPlayer <- function(plid) {
  # Get player info given the playerId
  subset(pl, id == plid, select=c(first_name, last_name, TeamName, League, Division))
}
getPlayerId <- function(plName) {
  subset(pl, last_name == plName)$id
}

dispPlot <- function(pitchId) {
  df <- subset(pac, pitcher == pitchId)
  oneMeds <- ddply(df, c('pitch_type', 'event'),
                   function(df) summarise(df, medPx = median(pxScale), medPz = median(pzScale),
                                          medHD = median(HorizDisp), medVD = median(VertDisp)))
  ggplot(subset(pac, pitcher == pitchId), 
         aes(HorizDisp, VertDisp)) + geom_point() + facet_grid(pitch_type ~ event) +
           geom_vline(data = meds, aes(xintercept = medHD, alpha = 0.5)) + 
           geom_hline(data = meds, aes(yintercept = medVD, alpha = 0.5)) +
           geom_vline(data = oneMeds, aes(xintercept = medHD, alpha = 0.5, colour = 'red')) + 
           geom_hline(data = oneMeds, aes(yintercept = medVD, alpha = 0.5, colour = 'red')) 
}

id <- getPlayerId('Lincecum')
startRelief$StartRelief <- factor(startRelief$StartRelief, levels = c('starter', 'relief'))

pac <- join(pac, startRelief)
bestPitch <- subset(pac, StartRelief == 'starter' & numInningsPitchedNorm > 0, 
                    select=c(pitcher, numInningsPitched, pctSwingStrike, ERA, pctSwingStrikeNorm, ERANorm))
bestPitch <- unique(bestPitch)
bestPitch.pss <- arrange(bestPitch, -pctSwingStrike)
bestss <- bestPitch.pss[1,]$pitcher

getPlayer(bestss)
dispPlot(bestss)
bestss

numBatters <- ddply(pd, 'pitcher', function(df) summarise(df, games=length(unique(df$game_id)), batters=length(unique(df$ubnum)), pitches=nrow(df)))
numBatters <- arrange(numBatters, desc(batters))
startRelief <- subset(pitcherDf, select=c(pitcher, StartRelief))
numBatters$pitcher <- as.factor(numBatters$pitcher)
numBatters <- join(numBatters, startRelief)
numBatters$StartRelief <- factor(numBatters$StartRelief, levels = c('starter', 'relief'))


# would be nice to facet this on starter/reliever
ggplot(numBatters, aes(games, batters, colour = pitches)) + geom_point() + facet_wrap(~ StartRelief) +
  xlab('Number of Games Played') + ylab('Number of Batters Faced') +
  opts(title='Starter & Reliever Differences')

top10pctSwingStrike <- arrange(bestPitch, -pctSwingStrike)[1:10, ]
bot10pctSwingStrike <- arrange(bestPitch, pctSwingStrike)[1:10, ]
top10ERA <- arrange(bestPitch, ERA)[1:10, ]
bot10ERA <- arrange(bestPitch, -ERA)[1:10, ]

top10ERA$eraRank <- 1:10
bot10ERA$eraRank <- -1:-10
top10pctSwingStrike$pssRank <- 1:10
bot10pctSwingStrike$pssRank <- -1:-10

bestAndWorst <- rbind(melt(top10ERA, 'pitcher'), melt(bot10ERA, 'pitcher'), 
                      melt(top10pctSwingStrike, 'pitcher'), melt(bot10pctSwingStrike, 'pitcher'))
bestAndWorst.wd <- dcast(bestAndWorst, pitcher ~ variable, mean)
bestAndWorst.wd <- arrange(bestAndWorst.wd, pctSwingStrike)
bestAndWorst.wd

pitchersToKeep  <- bestAndWorst.wd$pitcher
