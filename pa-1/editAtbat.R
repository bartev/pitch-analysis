# Bartev Vartanian
# 2012-02-05

# Edit atbat09.Rda data

CleanAtbatData <- function (ab) {
  # Clean up data
  # why remove this? (Error for count info in this game)
  ab <- subset(ab, game_id != '2009/08/08/flomlb-phimlb-1') 
  
  # edit game_id (remove slash & dash)
  ab$game_id <- gsub("[/-]", "", ab$game_id)
  
  # TODO
  # Convert atbat$score to 1 = TRUE, 0 = FALSE/NA
  # To convert to TRUE/FALSE, omit the as.numeric part
  
  # Convert atbat$score to TRUE/FALSE
  ab <- transform(ab, score = as.logical(!is.na(score)))
 
  # Get rid of NA's in home/away team runs
  ab$home_team_runs[is.na(ab$home_team_runs)] <- 0
  ab$away_team_runs[is.na(ab$away_team_runs)] <- 0
  return(ab)
}

ModifyAtbatData <- function (ab) {
  require(plyr)
  # Add unique batter id (game_id + num)
  ab$ubnum <- paste(ab$game_id, ab$num, sep='_')
  
  # Convert batter/pitcher id's to factors
  ab <- transform(ab, batter=as.factor(batter), pitcher=as.factor(pitcher))
   
  # Create new columns
  ab <- transform(ab, htRuns=0, atRuns=0, homeScore=0, awayScore=0)

  # for each game_id, find the currnt score for home & away teams
  temp <- subset(ab, select=c(game_id, home_team_runs, away_team_runs))
  temp <- ddply(temp, c('game_id'), function(x) transform(x, 
                                                      homeScore=cummax(home_team_runs),
                                                      awayScore=cummax(away_team_runs)))
    # using by & do.call(rbind, ...) is faster than ddply, even though more steps
    # I think ddply uses less memory
    abm <- by(ab[,c('home_team_runs', 'away_team_runs')], ab$game_id, function(d) apply(d, 2, cummax))
    # Combine output of 'by' into a dataframe (stack output together)
    abm <- as.data.frame(do.call(rbind, abm))
    # Add new columns to original data frame
    ab$homeScore <- abm$home_team_runs
    ab$awayScore <- abm$away_team_runs
  
  # See below for an alternate way to do this using ddply (plyr)
  # didn't work nearly as fast using ddply
  hs <- sapply(split(temp[, ('homeScore')], temp$game_id), function(v) diff(c(0, v)))
  as <- sapply(split(temp[, ('awayScore')], temp$game_id), function(v) diff(c(0, v)))
  ab$htRuns <- as.vector(unlist(hs))
  ab$atRuns <- as.vector(unlist(as))
  
  # Add a column for the number of runs scored on this at bat against the pitcher
  ab <- mutate(ab, 
                runsOnPitcher = atRuns * as.numeric(half == 'top') + 
                  htRuns * as.numeric(half == 'bottom'))

  # Add a column to show how many runs the pitcher is leading by.
  ab$pAhead <- (ab$homeScore - ab$awayScore)*2*(0.5 - as.numeric(ab$half=='bottom'))
  
  return(ab)
}

#### Alternate way of getting htRuns/atRuns using plyr
#### Takes too long with ab (23 variables)
#### Works ok with only a few variables
#### Replaces hs <- sapply... (4 lines of code above)
# g <- function(df) {
#   v <- df[, c('homeScore', 'awayScore')]
#   v <- rbind(0, v)
#   htRuns <- diff(v[,1])
#   atRuns <- diff(v[,2])
#   htRuns <- as.data.frame(htRuns)
#   atRuns <- as.data.frame(atRuns)
#   res <- cbind(htRuns, atRuns)
#   return(cbind(df, res))
# }
# r3 <- ddply(temp, c('game_id'), g)