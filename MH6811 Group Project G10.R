# Read training data
train.origin <- read.table('train.csv', sep = ",", stringsAsFactors = FALSE,na.strings=c(NA,''), header=TRUE)
summary(train.origin)
sum(is.na(train.origin))

# Handle missing values in training data
library(mice)
md.pattern(train.origin)
tempData <- mice(train.origin,m=5,maxit=50,meth='pmm')
summary(tempData)
completedData <- complete(tempData)
sapply(completedData, function(x) sum(is.na(x)))
train <- completedData
library(zoo)
train <- na.locf(na.locf(train),fromLast=TRUE)
sum(is.na(train))

# Read test data
test.origin <- read.table('test.csv', sep = ",", stringsAsFactors = FALSE,na.strings=c(NA,''), header=TRUE)
summary(test.origin)
sum(is.na(test.origin))

# Handle missing values in test data
md.pattern(test.origin)
tempData2 <- mice(test.origin,m=5,maxit=50,meth='pmm')
summary(tempData2)
completedData2 <- complete(tempData2)
sapply(completedData2, function(x) sum(is.na(x)))
test <- completedData2
test <- na.locf(na.locf(test),fromLast=TRUE)
sum(is.na(test))

# Train with knn
# Divide the train and test data sets for training with a ratio of 7:3
set.seed(114514)
sub<-sample(1:nrow(train),round(nrow(train)*7/10))
length(sub)
pd.train<-train[sub,]
pd.test<-train[-sub,]
pd.train.X <- pd.train[,c(2:12)]
pd.test.X <- pd.test[,c(2:12)]
pd.train.Y <- pd.train[,13]
pd.test.Y <- pd.test[,13]
library(CatEncoders)
t <- function(x) {
  # check if x is numeric
  if(is.numeric(x)) {
    return (x)
  }
  l <- LabelEncoder.fit(x)
  y <- transform(l, x)
  return (y)
}
knn.train.X <- sapply(pd.train.X, t)
knn.test.X <- sapply(pd.test.X, t)
library(class)
knn.pred <- knn(train = knn.train.X, test = knn.test.X, cl = pd.train.Y, k = 5)
table(knn.pred, pd.test.Y)
mean(knn.pred != pd.test.Y) #MSE
