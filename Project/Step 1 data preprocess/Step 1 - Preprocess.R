##### First half-----------------------------------------------------
# CS 699 Project
setwd("/Users/JisunLee/Desktop/Spring 2022/CS 699/Project")
getwd()
library(dplyr)
library(ggplot2)

df <- read.csv('nba_logreg.csv')
df
# first half
df_half <- df[c(1:11)]

# Checking missing values in entire data and each colume
table(df_half[is.na(df_half)])
## No missing values
df_nomiss <- na.omit(df_half)
df_nomiss

str(df_nomiss)

# Make a categories with dividing by first and third quartile. 
## GP: Games Played
boxplot(df_nomiss$GP, main = "Games Played")$stats
qrt3 <- fivenum(df_nomiss$GP)[4]
qrt1 <- fivenum(df_nomiss$GP)[2]

df_nomiss$GP<- ifelse(df_nomiss$GP > qrt3, "High", ifelse(df_nomiss$GP <= qrt1, "Low", "Moderate"))
table(df_nomiss$GP)

## MIN: Minutes Played
boxplot(df_nomiss$MIN, main = "Minutes Played")$stats
qrt3 <- fivenum(df_nomiss$MIN)[4]
qrt1 <- fivenum(df_nomiss$MIN)[2]

df_nomiss$MIN<- ifelse(df_nomiss$MIN > qrt3, "High", ifelse(df_nomiss$MIN <= qrt1, "Low", "Moderate"))
table(df_nomiss$MIN)

## PTS: Points Per Game
# don't preprocess the PTS column
boxplot(df_nomiss$PTS, main = 'PTS')

## FGM: Field Goals Made
# assign 'high' if the datas are outlier, which means the player makes lots of point
temp = boxplot(df_nomiss$FGM, main = "Field Goals Made")$stats
up <- temp[5] # upper limit of the boxplot
qrt1 <- fivenum(df_nomiss$FGM)[2]

df_nomiss$FGM<- ifelse(df_nomiss$FGM > up, "High", ifelse(df_nomiss$FGM <= qrt1, "Low", "Moderate"))
table(df_nomiss$FGM)

## FGA: Field Goal Attempts
# no need to preprocess Attempts
# FGM and FGPercentage are more important 
boxplot(df_nomiss$FGA, main = 'FGA')

## FG.: Field Goals Percentage
# assign 'high' if the datas are outlier, which means the play has a extra high FG Percentage
temp = boxplot(df_nomiss$FG., main = "Field Goals Percentage")$stats
up = temp[5] # upper limit of the boxplot
qrt1 <- fivenum(df_nomiss$FG.)[2]

df_nomiss$FG.<- ifelse(df_nomiss$FG. > up, "High", ifelse(df_nomiss$FG. <= qrt1, "Low", "Moderate"))
table(df_nomiss$FG.)

## X3P.Made: 3 Point Made
# assign 'high' if the datas are outlier, which means the player makes lots of point
temp = boxplot(df_nomiss$X3P.Made, main = "3 Point Made")$stats
up = temp[5]
qrt1 <- fivenum(df_nomiss$X3P.Made)[2]

df_nomiss$X3P.Made<- ifelse(df_nomiss$X3P.Made > up, "High", ifelse(df_nomiss$X3P.Made <= qrt1, "Low", "Moderate"))
table(df_nomiss$X3P.Made)

## X3PA: 3 Point Attempts
# no need to preprocess X3PA
# X3PM and X3PPercentage are more important 
boxplot(df_nomiss$X3PA, main = 'X3PA')

## X3P.: 3 Point Percentage
# assign 'high' if the datas are outlier, which means the play has a extra high FG Percentage
temp = boxplot(df_nomiss$X3P., main = "3 Point Percentage")$stats
up = temp[5]
qrt1 <- fivenum(df_nomiss$X3P.)[2]

df_nomiss$X3P.<- ifelse(df_nomiss$X3P. > up, "High", ifelse(df_nomiss$X3P. <= qrt1, "Low", "Moderate"))
table(df_nomiss$X3P.)

## FTM: Free Throw Made
boxplot(df_nomiss$FTM, main = "Free Throw Made")$stats
qrt3 <- fivenum(df_nomiss$FTM)[4]
qrt1 <- fivenum(df_nomiss$FTM)[2]

df_nomiss$FTM<- ifelse(df_nomiss$FTM > qrt3, "High", ifelse(df_nomiss$FTM <= qrt1, "Low", "Moderate"))
table(df_nomiss$FTM)

# create new csv file after categorized
write.csv(df_nomiss, "C:/Users/Lee/iCloudDrive/Document/Boston University/CS699 Data Mining/Team Project/firstHalf.csv")



##### Second half--------------------------------------------------------------------------------
# set working direction
setwd("C:/Users/Lee/iCloudDrive/Document/Boston University/CS699 Data Mining/Team Project/R Code")
getwd()

# read csv file
df = read.csv('nba_logreg.csv')
df = df[, c(1,12, 13, 14, 15, 16, 17, 18, 19, 20, 21)] # extract second half of columns
df[is.na(df)] = 0 # assign 0 to NA 
sum(is.na(df)) # no NA if 0
dfCleaned = df # copy one df for df cleaning
cat('total', length(df$Name), 'data rows')

##### Preprocess for each attribute
histWithQuatile = function(data, title){
  temp = fivenum(data)
  q1 = temp[2]
  q2 = temp[3]
  q3 = temp[4]
  hist(data, main = title)
  abline(v = c(q1, q2, q3), col = 'red')
  abline(v = mean(data), col = 'blue')
  return(list('q1'= q1, 'q2'= q2, 'q3'= q3, 'mu' = mean(data)))
}

# FTA, free throw attempt 
# no need to preprocess FTA
# FTM and FTPercentage are more important 
dfFTA = df[, 'FTA']
histWithQuatile(dfFTA, 'FTA')


# FT%, free throw percentage
# follow the 50-40-90 rule in NBA
# assign 'high' when FTA = (90, ]
# assign 'middle' when FTA = (q2, 90]
# assign 'low' when FTA = [,q2]
dfFTPercent = df[, 'FT.']
temp = histWithQuatile(dfFTPercent, 'FT.')
dfCleaned$FT.[dfCleaned$FT. > 90] = 'High'
dfCleaned$FT.[(dfCleaned$FT. > temp$q2) & (dfCleaned$FT. <= 90)] = 'Moderate'
dfCleaned$FT.[dfCleaned$FT. <= temp$q2] = 'Low'

table(dfCleaned$FT.)


# OREB, offensive rebounds
# it is harder for a player to catch rebound in offensive position
# assign 'outstanding' if OREB = (mu, ]
# assign 'normal' if not 'outstanding'
dfOREB = df[, 'OREB']
temp = histWithQuatile(dfOREB, 'OREB')
dfCleaned$OREB[dfCleaned$OREB > temp$mu] = 'outstanding'
dfCleaned$OREB[dfCleaned$OREB != 'outstanding'] = 'normal'

table(dfCleaned$OREB)

# DREB, defensive rebounds
# it is easier for a player to catch rebound in defensive position
# assign 'outstanding' if DREB = (q3, ]
# assign 'normal' if not 'outstanding'
dfDREB = df[, 'DREB']
temp = histWithQuatile(dfDREB, 'DREB')
dfCleaned$DREB[dfCleaned$DREB > temp$q3] = 'outstanding'
dfCleaned$DREB[dfCleaned$DREB != 'outstanding'] = 'normal'

table(dfCleaned$DREB)

# REB, total rebounds
# no action on total rebounds columns
dfREB = df[, 'REB']
histWithQuatile(dfREB, 'REB')



# AST, assists
# assign
dfAST = df[, 'AST']
temp = histWithQuatile(dfAST, 'AST')
dfCleaned$AST[dfCleaned$AST > temp$q3] = 'outstanding'
dfCleaned$AST[dfCleaned$AST != 'outstanding'] = 'normal'

table(dfCleaned$AST)


# STL, steals
# assign 'outstanding' if STL > 1
# assign 'normal' if others
dfSTL = df[, 'STL']
histWithQuatile(dfSTL, 'STL')
dfCleaned$STL[dfCleaned$STL > 1] = 'outstanding'
dfCleaned$STL[dfCleaned$STL <= 1] = 'normal'

table(dfCleaned$STL)



# BLK, blocks
# assign 'outstanding' if BLK = (mu, ]
# assign 'normal' if not 'outstanding'
dfBLK = df[, 'BLK']
temp = histWithQuatile(dfBLK, 'BLK')
dfCleaned$BLK[dfCleaned$BLK > temp$mu] = 'outstanding'
dfCleaned$BLK[dfCleaned$BLK != 'outstanding'] = 'normal'

table(dfCleaned$BLK)



# TOV, turn over aka mistake
# assign 'high' if TOV = (q3, ]
# assign 'middle' if TOV = (q1, q3]
# assign 'low' if TOV = [, q1]
dfTOV = df[, 'TOV']
temp = histWithQuatile(dfTOV, 'TOV')
dfCleaned$TOV[dfCleaned$TOV > temp$q3] = 'High'
dfCleaned$TOV[(dfCleaned$TOV > temp$q1) & (dfCleaned$TOV <= temp$q3)] = 'Moderate'
dfCleaned$TOV[dfCleaned$TOV <= temp$q1] = 'Low'

table(dfCleaned$TOV)



##### Create new columns
# 180 rules
# sum of the percentage of two point, three point, and free throw
df = read.csv('nba_logreg.csv')
df = df[, c('FG.', 'X3P.', 'FT.')]
df[is.na(df)] = 0 # assign 0 to NA 
sum(is.na(df)) # no NA value if 0

temp = apply(X = df, MARGIN = 1, FUN = sum) # sum three percentage attributes

dfCleaned$rule_180 = temp # add new column in dfCleaned

# STEAL - TURNOVER ratio
df = read.csv('nba_logreg.csv')
df = df[, c('STL', 'TOV')]
df[is.na(df)] = 0 # assign 0 to NA 
sum(is.na(df)) # no NA value if 0

temp = df$STL / df$TOV # make steal-turnover ratio

dfCleaned$STL_TOV_ratio = temp # add new column in dfCleaned

# double check
# check how many DOUBLE does a player achieve
df = read.csv('nba_logreg.csv')
df = df[, c('PTS', 'REB', 'BLK', 'AST', 'STL')]
df[is.na(df)] = 0 # assign 0 to NA 
sum(is.na(df)) # no NA value if 0

df$PTS = ifelse(df$PTS >= 10, TRUE, FALSE) # check if double
df$REB = ifelse(df$REB >= 10, TRUE, FALSE) # check if double
df$BLK = ifelse(df$BLK >= 10, TRUE, FALSE) # check if double
df$AST = ifelse(df$AST >= 10, TRUE, FALSE) # check if double
df$STL = ifelse(df$STL >= 10, TRUE, FALSE) # check if double

temp = apply(X = df, MARGIN = 1, FUN = sum) # sum of doulbe times
dfCleaned$double_time = temp # add a new column

# player efficiency
# https://captaincalculator.com/sports/basketball/efficiency/
# Efficiency (EFF) = (PTS + REB + AST + STL + BLK - Missed FG - Missed FT - TOV)
# Efficiency per Game = EFF ÷ Games Played
# Efficiency per 48 minutes (EFF/48)= (EFF ÷ Minutes Played) x 48
df = read.csv('nba_logreg.csv')
df[is.na(df)] = 0 # assign 0 to NA 
sum(is.na(df)) # no NA value if 0

# count EEF
temp_EEF = df$PTS+df$REB+df$AST+df$STL+df$BLK - (df$FGA - df$FGM) - (df$FTA - df$FGM) - df$TOV
dfCleaned$EEF = temp_EEF

# count EEF per game
temp_EEF_GP = temp_EEF / df$GP
dfCleaned$EEF_GP = temp_EEF_GP

# count EEF per minutes
temp_EEF_MIN = temp_EEF / df$MIN
dfCleaned$EEF_MIN = temp_EEF_MIN


##### output csv
write.csv(dfCleaned, 'C:/Users/Lee/iCloudDrive/Document/Boston University/CS699 Data Mining/Team Project/secondHalf.csv')

##### Combine two csv file------------------------------------------------------------------------------------------
df1 = read.csv('firstHalf.csv')
df2 = read.csv('secondHalf.csv')
df3 = merge(x = df1, y = df2, by = 'Name')
df3 = subset(x = df3, select = -c(X.x, X.y, Name))

# turn 3 order into numeric
df3[df3 == 'Moderate'] = 2
df3[df3 == 'Low'] = 1
df3[df3 == 'High'] = 3
# turn 2 order into numeric
df3[df3 == 'outstanding'] = 2
df3[df3 == 'normal'] = 1

# save csv
write.csv(df3, 'C:/Users/Lee/iCloudDrive/Document/Boston University/CS699 Data Mining/Team Project/finalPreprocessed.csv' )

