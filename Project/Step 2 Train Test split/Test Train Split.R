setwd('C:/Users/Lee/iCloudDrive/Document/Boston University/CS699 Data Mining/Team Project/Step 2 Train Test split')
getwd()

# read csv
df = read.csv('finalPreprocessed.csv')

# train test split
n = nrow(df)
testNumber = rep(0, n * 0.34) # 0 is test
trainNumber = rep(1, n * 0.66) # 1 is train
temp = sample(c(testNumber, trainNumber))

dfTest = df[temp == 0, ]
dfTrain = df[temp == 1, ]

prop.table(table(dfTest$TARGET_5Yrs)) # check balance 
table(dfTest$TARGET_5Yrs)
      
prop.table(table(dfTrain$TARGET_5Yrs)) # check balance
table(dfTrain$TARGET_5Yrs)

write.csv(dfTest, file = 'testing set.csv')
write.csv(dfTrain, file = 'training set.csv')



