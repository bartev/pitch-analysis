# 2012-02-16
# Look for relationship between spin_dir, spin_rate, pfx_x, pfx_z
# Results:
# It looks as though spin_dir is 0/360 at the top, so spin_dir=0 refers 
# to top spin, resulting in a drop in the ball's position (pfx_z < 0)
# The horizontal component of the spin (sin(spin_dir)) contributes to the
# horizontal displacement of the ball (pfx_x)

# Edit spin_dir & spin_rate to find useful relationships


# spin_dir is given in degrees. Use trig functions in degrees instead of rad
# Capital letters use degrees, lowercase {base} us radian
Sin <- function(theta) sin(theta * pi / 180)
Cos <- function(theta) cos(theta * pi / 180)

df <- subset(pd, select=c(pfx_x, pfx_z, break_y, break_angle, break_length, spin_dir, spin_rate))
# Convert angles s.t. 0 degrees is horizontal, 90 is straight up, 180 horiz
# and negative degrees indicate back spin
df <- mutate(df, SpinAngle = (spin_dir + 270) %% 360 - 180)
# Positive SpinAngle indicates top spin, negative ~ bottom spin (e.g. fast ball)

df.spin <- summarize(df, SpinAngle, 
                     VertSpin = Sin(SpinAngle) * spin_rate,
                     HorizSpin = Cos(SpinAngle) * spin_rate,
                     VertDisp = pfx_z,
                     HorizDisp = pfx_x
                     )

# Higher spin_rate => more displacement (horiz or vert)
ggplot(data=df, aes(x=pfx_x, y=pfx_z, colour=spin_rate)) + geom_point()

# V shaped plot - spin rate vs z movement
ggplot(data=df, aes(pfx_z, spin_rate, colour=spin_dir)) + geom_point()


######
### Straight lines ####

# Vertical displacement
# Interesting plots - almost straight lines
ptitle <- opts(plot.title = theme_text(size = 14, lineheight = 1.2, face = 'bold'))
toplot <- sample(1:nrow(df.spin), 5000)

topts <- opts(title = 'Vertical Displacement With Spin')
vspin <- ggplot(df.spin[toplot,], aes(VertSpin, VertDisp, colour=SpinAngle)) 
vspin <- vspin + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(0)) +
  topts + ptitle +
  ylab('Vertical Displacement (in)') +
  xlab('Vertical Component of Spin Rate') +
  scale_colour_continuous(name = 'Spin Angle', 
                          breaks=c(-179, -90, 0, 90, 179),
                          labels = c('-180°', '-90°', '0°', '90°', '180°')
                          )
vspin

topts <- opts(title = 'Horizontal Displacement With Spin')
hspin <- ggplot(df.spin[toplot,], aes(HorizSpin, HorizDisp, colour=SpinAngle)) 
hspin <- hspin + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(0)) +
  topts + ptitle +
  ylab('Horizontal Displacement (in)') +
  xlab('Horizontal Component of Spin Rate') +
  scale_colour_continuous(name = 'Spin Angle', 
                          breaks=c(-179, -90, 0, 90, 179),
                          labels = c('-180°', '-90°', '0°', '90°', '180°')
                          )
hspin

# Quick versions of above plots
# Vertical displacement
# qplot(Sin(SpinAngle) * spin_rate, pfx_z, data=df, geom='point', colour = SpinAngle)
# Horizontal displacement
# qplot(Sin(spin_dir) * spin_rate, pfx_x, data=df, geom='point', colour=spin_dir)


######
### Coloring shows ball movement with spin direction/rate ####
# Effect of Sin/Cos(spin_dir) * spin_rate on pfx_z ~ pfx_x
# ggplot(data=df, aes(x=pfx_x, y=pfx_z, colour=Cos(spin_dir) * spin_rate)) + geom_point()
# ggplot(data=df, aes(x=pfx_x, y=pfx_z, colour=Sin(spin_dir) * spin_rate)) + geom_point()


## Explore fit
## Add fit lines
# Both horiz and vertical displacements have the same fit.
# Are these calculated values?
# > spin.vert.fit$coefficients
# VertSpin 
# -0.00520346 
# > spin.horiz.fit$coefficients
# HorizSpin 
# -0.005307634 

spin.vert.fit <- lm(VertDisp ~ VertSpin - 1, data=df.spin)
vspin + geom_abline(intercept = 0, 
                    slope = coefficients(spin.vert.fit), 
                    colour = 'red')

spin.horiz.fit <- lm(HorizDisp ~ HorizSpin - 1, data=df.spin)
hspin + geom_abline(intercept = 0, 
                    slope = coefficients(spin.horiz.fit), 
                    colour = 'red')


## Examine Spin Angle vs displacement
pitchesToKeep <- c('FF', 'SL', 'CH', 'CU', 'FT')
df <- subset(pd, pitch_type %in% pitchesToKeep,
             select=c(pitcher, pitch_type, pfx_x, pfx_z, spin_dir, spin_rate))
df <- mutate(df, SpinAngle = (spin_dir + 270) %% 360 - 180)
meltDf <- melt(df, id.vars=c('pitcher', 'pitch_type', 'spin_dir', 'spin_rate', 'SpinAngle'))

# Some cool sinusoidal plots
library(scales)
ggplot(meltDf, aes(SpinAngle, value) ) + geom_point(alpha=1/20) + facet_grid(pitch_type ~ variable) +
  scale_x_continuous(limits=c(-180, 180), breaks = seq(-180, 180, 90)) +
  xlab('Spin Angle (0ª = down, 90ª = left of pitcher, top spin)') +
  ylab('Deflection') +
  opts(title = 'Deflection due to Spin Angle')
ggsave(file='DeflSpinAngle.pdf', dpi=72) ## HUGE FILE! 77 MB
ggplot(meltDf, aes(spin_dir, value) ) + geom_point(alpha=1/20) + facet_grid(pitch_type ~ variable) +
  scale_x_continuous(limits=c(0, 360), breaks = seq(0, 360, 90)) +
  xlab('spin_dir (270ª = down, 0ª = left of pitcher, top spin)') +
  ylab('Deflection') +
  opts(title = 'Deflection due to spin_dir')


# USED IN PAW
ggplot(meltDf, aes(SpinAngle, value) ) + geom_hex() + facet_grid(pitch_type ~ variable) +
  scale_x_continuous(limits=c(-180, 180), breaks = seq(-180, 180, 90)) +
  xlab('Spin Angle (0º = down, 90º = left of pitcher, top spin)') +
  ylab('Deflection') +
  opts(title = 'Deflection due to Spin Angle') + ptitle
