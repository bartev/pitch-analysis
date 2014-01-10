# Bartev Vartanian
# 2012-02-05

# Start here to do everything
rm(list=ls())
mainDir <- '~/Rmac/Pitchfx/'
dataDir <- '~/Rmac/Pitchfx/pitchfxData/'


# Load all data
setwd(dataDir)


# Load fresh data, clean, edit
setwd(mainDir)
system.time( source('loadPfxData.R') )
### OR ###
setwd(dataDir)
load('pfxData09.Rdat')    # pitchfx '09 data - full monty
load('pfxDataSample.Rdat')  # 5000 rows max of pitchfx '09 data
setwd(mainDir)


