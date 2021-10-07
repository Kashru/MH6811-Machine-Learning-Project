# Read data
train.origin <- read.table('train.csv', sep = ",", stringsAsFactors = FALSE,na.strings=c(NA,''), header=TRUE)
summary(train.origin)
sum(is.na(train.origin))
# Handle missing values
library(mice)
md.pattern(train.origin)
tempData <- mice(train.origin,m=5,maxit=50,meth='pmm')
summary(tempData)
completedData <- complete(tempData)
sapply(completedData, function(x) sum(is.na(x)))
train <- completedData
