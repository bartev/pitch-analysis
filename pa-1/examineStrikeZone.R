# Before editing pitch data, examine the strike zone data

# szData is from loadPfxData.R
require(ggplot2)
source('fnUtilityFunctions.R')
  # includes function multiplot



# Data before editing
szData <- transform(szData, sz_range=(sz_top - sz_bot))
szData.o <- szData[order(szData$sz_range),]
szData.o$pitchNum <- 1:nrow(szData.o)

p1 <- ggplot(data=szData.o, aes(x=pitchNum, y=sz_bot)) + geom_point(colour='blue', alpha=I(1/20)) +
  geom_point(aes(y=sz_top), colour='red', alpha=I(1/20))
p2 <- ggplot(data=szData.o, aes(x=pitchNum, y=sz_range)) + geom_point(colour='green', alpha=I(1/20))

multiplot(p1, p2, cols=1)

# Data after editing
szData.e <- transform(szData.e, szRange=(szTop - szBot))
szData.e.o <- szData.e[order(szData.e$szRange),]
szData.e.o$pitchNum <- 1:nrow(szData.e.o)

p3 <- ggplot(data=szData.e.o, aes(x=pitchNum, y=szBot)) + geom_point(colour='darkblue', alpha=I(1/20)) +
  geom_point(aes(y=szTop), colour='darkred', alpha=I(1/20))
p4 <- ggplot(data=szData.e.o, aes(x=pitchNum, y=szRange)) + geom_point(colour='darkgreen', alpha=I(1/20))

multiplot(p3, p4, cols=1)

