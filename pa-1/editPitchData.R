# Bartev Vartanian
# 2012-02-05

# Edit pitch09 data

CleanPitchData <- function (pData) {
  # Clean the data - this game did not reset the count to 0-0 for each batter. 
  # b & s columns are all off
  # Bad data starting ~line 300 in this game until almost the end
  pData <- subset(pData, game_id != '2009/08/08/flomlb-phimlb-1')
  pData <- transform(pData, batter = as.factor(batter))
  
  # Clean up NA's in data
  # 8522 lines with NA's in pp$sv_id (& many other fields) (~ 1% of the data)
  # 1294 unique game_id's in the 8522 lines
  ### Warning - discarding data here!
  pData <- pData[which(!(is.na(pData$sv_id))),]
  
  pData <- transform(pData, 
                     on_1b=ifelse(is.na(on_1b), FALSE, TRUE), 
                     on_2b=ifelse(is.na(on_2b), FALSE, TRUE), 
                     on_3b=ifelse(is.na(on_3b), FALSE, TRUE))
  
  #   pData$on_1b[which(is.na(pData$on_1b))] <- 0
  #   pData$on_2b[which(is.na(pData$on_2b))] <- 0
  #   pData$on_3b[which(is.na(pData$on_3b))] <- 0
  
  return(pData)
}

ModifyPitchData <- function (pData) {
  require(plyr)
  source('fnUtilityFunctions.R')
  # Add row numbers
  pData$seasonPitchNum <- 1:nrow(pData)
  
  # adjust strike zone to be within q1 & q3
  # replace NA values with mean for batter (stored in szBotMMQ/szTopMMQ)
  szBotMMQ <- ddply(pData, c('batter'), function(x) MinMaxQuant(x, 'sz_bot'), .progress='text')
  szTopMMQ <- ddply(pData, c('batter'), function(x) MinMaxQuant(x, 'sz_top'), .progress='text')
  pData <- GetStrikeZoneTopBottomInRange(pData, szBotMMQ, szTopMMQ)
  
  # Scale columns for strike zone
  maxX <- 17/12/2
  pData <- transform(pData, 
                     pxScale = (px / maxX),
                     pzScale = 2 * (pz - (szTop + szBot) / 2) / (szTop - szBot)
                     )
  pData <- transform(pData, 
                     sZone=PitchZone(pData),
                     inZone=(abs(pxScale) <= 1 & abs(pzScale) <= 1)
                     )
  
  # Add team, League, Division to the pitch09 data
  # May not be very good because players may have been traded
  pl <- subset(players09, select=c(id, team, League, Division))
  pData <- merge(pData, pl, by.x='pitcher', by.y='id')

  pData <- pData[order(pData$seasonPitchNum),]
  
  # Add the count to use as a factor later
  # Count has 12 factors (ball strike): "0 0", "0 1", ...
  pData$count <- as.factor(paste(pData$b, sapply(pData$s, function(s) min(s, 2))))

  # Add pitch number per batter (defined below in helper functions)
  pData <- PitchNumPerBatter(pData)
  
  # edit game_id (remove slash & dash)
  pData$game_id <- gsub("[/-]", "", pData$game_id)

  # Add unique pitch & bat id's
  pData <- transform(pData, 
                  upid=paste(game_id, id, sep='_'), 
                  ubnum=paste(game_id, num, sep='_')
                  )

  return(pData)
}


###### Helper functions

GetStrikeZoneTopBottomInRange <- function(df, szBotMMQ, szTopMMQ) {
  # given a data frame (df) which in includes batter, sz_bot & sz_top, 
  # this function will merge the df's szBotMMQ & szTopMMQ resulting from
  # MinMaxQuant,
  # Change NA -> the median value
  # Change values < 25th percentile to 25th percentile value
  # Change values > 75th percentile to 75th percentile value
  df <- merge(df, szBotMMQ)
  df <- merge(df, szTopMMQ)
  df <- transform(df, 
                  sz_bot=ifelse(is.na(sz_bot), sz_bot_q2, sz_bot),
                  sz_top=ifelse(is.na(sz_top), sz_top_q2, sz_top)
                  )
  df <- transform(df, 
                  szBot = pmin(pmax(sz_bot, sz_bot_q1, na.rm=TRUE), sz_bot_q3, na.rm=TRUE),
                  szTop = pmin(pmax(sz_top, sz_top_q1, na.rm=TRUE), sz_top_q3, na.rm=TRUE)
                  )
  # remove extra 10 columns that we added (szTop/szBot min, max, q1, q2, q3)
  numcols <- ncol(df)

  # remove extra columns
  root <- c('min', 'max', 'q1', 'q2', 'q3')
  drops <- c(paste('sz_bot', root, sep='_'), paste('sz_top', root, sep='_'), 'sz_bot', 'sz_top')
  df <- df[, !(names(df) %in% drops)]
  return(df)
}

PitchNumPerBatter <- function(pitchDf) {
  # Running sum of pitches for each batter
  # Adds the columns pitchNum and finalPitch (bool) to the pitchDf
  # Returns a df with the added columns.
  # 
  # For each batter number in a game (atbat), number the  pitches taken
  # Input is a df with game_id, and num (at bat number)
  # Got some great help from stack overflow. Cut the time from hours to seconds for 700k rows.
  # Combine fun1 & fun2 methods
  # df <- sqldf('select game_id, num from pitchDf') 
  # selecting the 2 columns is MUCH faster than using sqldf
  df <- pitchDf[, c('game_id', 'num')]
  #   Create new column
  df$pitchNum <- 0
  df$finalPitch <- FALSE
  
  zz <- rle(as.character(df$num))$lengths
  df$pitchNum <- sequence(zz)  # create sequences from 1:length for each batter
  df$finalPitch[head(cumsum(zz), -1 )]  <- TRUE  # cumsum(zz) gives starting location of each batter
  
  df <- cbind(pitchDf, pitchNum = df$pitchNum, finalPitch = df$finalPitch)
  return(df)
}

PitchZone <- function(df) {
  # given a df with scaled positins pxScale, pzScale (pitch position scaled
  # for width of plate & top/bottom of strike zone), return the "zone"
  # for the pitch.
  # Strike zone = 1-9 (3x3 grid, numbered from top left going across)
  # Balls - 4 quadrants 10-13 numbered from top left
  #  10       11
  #    1  2  3
  #    4  5  6        Scaled strike zone (x, z > 1 is a ball)
  #    7  8  9
  #  12       13
  x <- df$pxScale
  z <- df$pzScale
  zone <- ifelse( x < -1/3, 1, ifelse( x < 1/3, 2, 3))
  mult <- ifelse( z < -1/3, 3, ifelse( z < 1/3, 2, 1))
  zone <- mult * zone
  ball <- abs(x) > 1 | abs(z) > 1
  quad <- ifelse(x < 0, ifelse(z < 0, 12, 10), ifelse(z < 0, 13, 11))
  zone <- ifelse(ball, quad, zone)
  return (zone)
}