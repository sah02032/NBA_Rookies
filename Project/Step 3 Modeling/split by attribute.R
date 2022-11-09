setwd('C:/Users/Lee/iCloudDrive/Document/Boston University/CS699 Data Mining/Team Project/Step 2 Train Test split')
getwd()

# read csv
dfTrain = read.csv('training set.csv')
dfTest = read.csv('testing set.csv')



# subset Train data into five branches
# A1: EEF + PTS + FTA + EEF_MIN + FGA
# A2: GP + FTM + MIN + FG. + TOV
# A3: AST + BLK + DREB + OREB + STL
# A4: GP + FTM + OREB + MIN + FG.
# A5: EEF + GP + FTM + PTS + FTA

A1Train = dfTrain[, c('EEF', 'PTS', 'FTA', 'EEF_MIN', 'FGA', 'TARGET_5Yrs')]
write.csv(A1Train, file = 'A1Train.csv', row.names = FALSE)

A2Train = dfTrain[, c('GP', 'FTM', 'MIN', 'FG.', 'TOV', 'TARGET_5Yrs')]
write.csv(A2Train, file = 'A2Train.csv', row.names = FALSE)

A3Train = dfTrain[, c('AST', 'BLK', 'DREB', 'OREB', 'STL', 'TARGET_5Yrs')]
write.csv(A3Train, file = 'A3Train.csv', row.names = FALSE)

A4Train = dfTrain[, c('GP', 'FTM', 'OREB', 'MIN', 'FG.', 'TARGET_5Yrs')]
write.csv(A4Train, file = 'A4Train.csv', row.names = FALSE)

A5Train = dfTrain[, c('EEF', 'GP', 'FTM', 'PTS', 'FTA', 'TARGET_5Yrs')]
write.csv(A5Train, file = 'A5Train.csv', row.names = FALSE)



# subset Test data into five branches
# A1: EEF + PTS + FTA + EEF_MIN + FGA
# A2: GP + FTM + MIN + FG. + TOV
# A3: AST + BLK + DREB + OREB + STL
# A4: GP + FTM + OREB + MIN + FG.
# A5: EEF + GP + FTM + PTS + FTA
A1Test = dfTest[, c('EEF', 'PTS', 'FTA', 'EEF_MIN', 'FGA', 'TARGET_5Yrs')]
write.csv(A1Test, file = 'A1Test.csv', row.names = FALSE)

A2Test = dfTest[, c('GP', 'FTM', 'MIN', 'FG.', 'TOV', 'TARGET_5Yrs')]
write.csv(A2Test, file = 'A2Test.csv', row.names = FALSE)

A3Test = dfTest[, c('AST', 'BLK', 'DREB', 'OREB', 'STL', 'TARGET_5Yrs')]
write.csv(A3Test, file = 'A3Test.csv', row.names = FALSE)

A4Test = dfTest[, c('GP', 'FTM', 'OREB', 'MIN', 'FG.', 'TARGET_5Yrs')]
write.csv(A4Test, file = 'A4Test.csv', row.names = FALSE)

A5Test = dfTest[, c('EEF', 'GP', 'FTM', 'PTS', 'FTA', 'TARGET_5Yrs')]
write.csv(A5Test, file = 'A5Test.csv', row.names = FALSE)
