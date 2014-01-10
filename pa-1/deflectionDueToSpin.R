# Diagram with 6 different pitch locations
mainDir <- '~/Rmac/Pitchfx/'
setwd(mainDir)
source('loadLotsaData.R')

library(hexbin)



typicalPitches <- subset(sgls, throws == 'R' & pitch_type %in% pitchesToKeep,
                         select = c(pitcher, pitch_type, pfx_x, pfx_z, px, pz,
                                    start_speed, end_speed, spin_dir, spin_rate,
                                    szBot, szTop, pxScale, pzScale))
somePitches <- ddply(typicalPitches, 'pitch_type', 
                     function(df) df[sample(1:nrow(df), 20, replace=FALSE),])

ggplot(somePitches, aes(pfx_x, pfx_z, colour = start_speed)) + geom_point(size=6) + facet_wrap(~ pitch_type) +
  geom_vline(x=0, colour='grey40') + geom_hline(yintercept=0, colour = 'grey40') +
  xlab('x deflection (inches)') + ylab('z deflection (inches)') +
  opts(title = 'Deflection Due To Spin for Changeup, Curve, \nFast, Four-Seamer, Two-Seamer, Slider (RH Pitcher)') +
  labs(colour = 'Start Speed (mph)')


pl.Lincecum <- subset(pl, last_name == 'Lincecum')
lincecumId <- pl.Lincecum$id

cnt <- count(pd, 'pitcher')
cnt <- arrange(cnt, -freq)
head(cnt)

# top 3 pitchers
id.3.pitchers <- cnt[1:3, 'pitcher']
pitcher.3 <- data.frame(id = id.3.pitchers)
pitcher.3 <- join(pitcher.3, subset(pl, select=c(id, last_name, first_name, throws, TeamName)))
names(pitcher.3)[1] <- 'pitcher'

top.3.typicalPitches <- subset(pd, pitcher %in% pitcher.3$pitcher & pitch_type %in% pitchesToKeep,
                               select = c(pitcher, pitch_type, pfx_x, pfx_z, px, pz,
                                          start_speed, end_speed, spin_dir, spin_rate,
                                          szBot, szTop, pxScale, pzScale))
top.3.typicalPitches <- join(top.3.typicalPitches, pitcher.3)
top.3.typicalPitches <- subset(top.3.typicalPitches, pitch_type != 'FA')

# USED IN PAW
ggplot(top.3.typicalPitches, aes(pfx_x, pfx_z, colour = start_speed)) + geom_point(size=4) + facet_grid(last_name ~ pitch_type) +
  geom_vline(x=0, colour='grey40') + geom_hline(yintercept=0, colour = 'grey40') +
  xlab('x deflection (inches)') + ylab('z deflection (inches)') +
  opts(title = 'Deflection Due To Spin for Changeup,\nCurve, Four-Seamer, Two-Seamer, Slider') + ptitle +
  labs(colour = 'Start Speed (mph)') +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0))


ggplot(top.3.typicalPitches, aes(pfx_x, pfx_z)) + geom_hex() + facet_grid(last_name ~ pitch_type) +
  geom_vline(x=0, colour='grey40') + geom_hline(yintercept=0, colour = 'grey40') +
  xlab('x deflection (inches)') + ylab('z deflection (inches)') +
  opts(title = 'Deflection Due To Spin for Changeup, Curve, Four-Seamer, Two-Seamer, Slider') +
  labs(colour = 'Start Speed (mph)') +
  opts(axis.text.x=theme_text(angle = 90, hjust = 0),
       legend.title=theme_text(angle = 90))


ggplot(top.3.typicalPitches, aes(px, pz)) + geom_hex() + facet_grid(last_name ~ pitch_type) + 
  
# USED IN PAW  
ggplot(top.3.typicalPitches, aes(pxScale, pzScale)) + geom_hex() + facet_grid(last_name ~ pitch_type) +
  geom_vline(x=0, colour='grey40') + geom_hline(yintercept=0, colour = 'grey40') +
  xlab('x Position (Strike Zone)') + ylab('z Position') +
  opts(title = 'Pitch Location (Strike Zone Normalized To +/- 1)') + ptitle
  