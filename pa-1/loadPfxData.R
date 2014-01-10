# Bartev Vartanian
# 2012-02-05

# Start here to load PitchFx data from '09 into local variables
library(sqldf)

# start fresh
rm(list=ls())
mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'

# Load all data
setwd(dataDir)
setwd(mainDir)

########## players09
# Data on each player
load(paste(dataDir,'players09.Rda', sep=""))
# head(players09)

# List of team names, League, Division, Pfx abbreviation, MLB abbreviation, city/state, ballpark name
teamNames <- read.csv(paste(dataDir, 'TeamNames.csv', sep=""))
teamNamesShort <- subset(teamNames, select=c(League, Division, PfxTeam, TeamName))

# Add League, Division, TeamName to players
# colNames <- c("team", "id", "League")
players09 <- sqldf("select * from players09, teamNamesShort where team = PfxTeam")
# Clean up
rm (teamNames, teamNamesShort)

########## atbat09
# Data on each atbat (1 line per batter)
load(paste(dataDir,'atbat09.Rda', sep=""))

source('editAtbat.R')
ab <- CleanAtbatData(atbat09)
ab <- ModifyAtbatData(ab)

########## pitch09
# Data on each pitch of each game - 717,409 rows
load(paste(dataDir,'pitch09.Rda', sep=""))

source('editPitchData.R')
pd <- CleanPitchData(pitch09)
# Before editing pitch data, examine the strike zone data
# szData <- subset(pd, select=c('batter', 'sz_bot', 'sz_top'))
pd <- ModifyPitchData(pd)
# szData.e <- subset(pd, select=c('batter', 'seasonPitchNum', 'szBot', 'szTop'))
# Clean up
# rm(szData, szData.e, szData.o, szData.e.o)

########## combine data from atbat09 & pitch09
# Use atbat09 to find home/away
# Couldn't use player data because 44k rows were neither home nor away - perhaps
# due to trading players midseason
up <- unique(subset(ab, select=c(game_id, half, pitcher)))
up$home <- up$half == 'top'
pd <- merge(pd, up)
pd <- pd[order(pd$seasonPitchNum),]
# Clean up
rm(up)


####### Save to files, clean up
ab09 <- ab
pd09 <- pd
pl09 <- players09
pl <- pl09

setwd(dataDir)
save(ab09, pd09, pl09, file='pfxData09_Cleaned.Rdat')  # Keep clean
save(ab, pd, pl, file='pfxData09.Rdat')   # Working copy

# Use a subset while testing stuff
atbat <- ab09[1:5000,]
pitch <- pd09[1:5000,]
players <- pl09
save(atbat, pitch, players, file='pfxDataSample.Rdat')  # Samples of data
setwd(mainDir)

rm(ab09, pd09, pl09)
# rm(ab, pd, pl)
rm(atbat09, pitch09, players09)
rm(atbat, pitch, players)
rm(GetStrikeZoneTopBottomInRange, ModifyAtbatData, ModifyPitchData, PitchNumPerBatter, PitchZone)
rm(CleanAtbatData, CleanPitchData)
# rm(list=ls())