setwd("C:/Users/Lee/iCloudDrive/Document/Boston University/CS699 Data Mining/Team Project")
getwd()
library(ggplot2)

##### read csv------------------------------------------------------------------------
df = read.csv('finalPreprocessed.csv')
head(df)



##### three ordinal attribute----------------------------------------------------------
plotTreeAttribute = function(columnName){
  temp = prop.table(table(df[, columnName], df$TARGET_5Yrs), margin = 2) * 100
  
  jpeg(filename = paste(columnName, '.jpg', sep = '')) # open of saving device
  
  barplot(height = temp, 
          beside = TRUE, 
          axes = TRUE, 
          xlab = 'TARGET CLASS', 
          ylab = 'Frequency %',
          ylim = c(0, 100),
          col = rainbow(3),
          main = columnName
  )
  legend('topright', legend = rownames(temp), fill = rainbow(3))
  
  dev.off() # close the plot saving device and save the image image
}

# GP
plotTreeAttribute('GP')
# MIN
plotTreeAttribute('MIN')
# FGM
plotTreeAttribute('FGM')
# FG.
plotTreeAttribute('FG.')
# X3P.Made
plotTreeAttribute('X3P.Made')
# X3P.
plotTreeAttribute('X3P.')
# FTM
plotTreeAttribute('FTM')
# FT.
plotTreeAttribute('FT.')
# TOV
plotTreeAttribute('TOV')

##### two ordinal attribute-------------------------------------------------------------
plotTwoAttribute = function(columnName){
  temp = prop.table(table(df[, columnName], df$TARGET_5Yrs), margin = 2) * 100
  
  jpeg(filename = paste(columnName, '.jpg', sep = '')) # open of saving device
  
  barplot(height = temp, 
          beside = TRUE, 
          axes = TRUE, 
          xlab = 'TARGET CLASS', 
          ylab = 'Frequency %',
          ylim = c(0, 100),
          col = rainbow(2),
          main = columnName
  )
  legend('topright', legend = rownames(temp), fill = rainbow(2))
  
  dev.off() # close the plot saving device and save the image
}

# OREB
plotTwoAttribute('OREB')
# DREB
plotTwoAttribute('DREB')
# AST
plotTwoAttribute('AST')
# STL
plotTwoAttribute('STL')
# BLK
plotTwoAttribute('BLK')

##### numeric data---------------------------------------------------------------------
library(plotly)
histOverlay = function(columnName){
  a = df[, columnName][df$TARGET_5Yrs == 1]
  b = df[, columnName][df$TARGET_5Yrs == 0]

  
  
  fig = plot_ly(alpha = 0.6)
  fig = fig %>% add_histogram(x = a, name = 'Class 1', histnorm = 'probability')
  fig = fig %>% add_histogram(x = b, name = 'Class 0', histnorm = 'probability')
  fig = fig %>% layout(barmode = 'overlay', title = columnName)
  fig
  
}

# PTS
histOverlay('PTS')
# FGA
histOverlay('FGA')
# X3PA
histOverlay('X3PA')
# FTA
histOverlay('FTA')
# REB
histOverlay('REB')
# rule_180
histOverlay('rule_180')
# STL_TOV_ratio
histOverlay('STL_TOV_ratio')
# double_time  
histOverlay('double_time')
# EEF     
histOverlay('EEF')
# EEF_GP   
histOverlay('EEF_GP')
# EEF_MIN
histOverlay('EEF_MIN')





