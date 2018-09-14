#preprocessing data

#read data

train = read.csv(file = 'train.csv')
test = read.csv(file = 'test.csv')
library(onehot)

#Split apart target variables for train set
ytrain = train$SalePrice


#what are the data types for each column?
#I am converting columns I want as categorical to factor

types = sapply(train, class)

coltofactor <- c("MSSubClass", 'OverallQual', 'OverallCond', 'MoSold')
train[,coltofactor] <- data.frame(apply(train[coltofactor], 2, as.factor))
test[,coltofactor] <- data.frame(apply(test[coltofactor], 2, as.factor))

#id total missing values
empty = is.na(train)
table(empty)
#6965 missing values

#find out which columns have missing values
missingdata = list()

cols = colnames(train)
for (x in cols){
  if((sum(is.na(train[x])))!=0){
    missingdata = c(missingdata, x)
  }
}
missingdata


#fill in na

#we will fill in na for integer columns as zero.  Missing categoricals will be handled with one hot encoding as own column
train[is.na(train)] = 0

#one hot encoding

traindummies = onehot(train, stringsAsFactors = FALSE, addNA = TRUE, max_levels = 100)
traindummies[length(traindummies)] <- NULL #removed sale price column so test and train are equal

trainmatrix<- predict(traindummies, train)
testmatrix<- predict(traindummies, test)

traindf = as.data.frame(trainmatrix)
testdf = as.data.frame(testmatrix)

#make negatives zero
traindf[traindf < 0] <- 0 
testdf[testdf < 0 ] <- 0

#normalize data

#concatenate test and train data so normalized over same data
combined = rbind(traindf, testdf)

rescale <- function(x){(x-min(x))/(max(x)-min(x))}

cols2 = colnames(combined)
for (col in cols2){
  combined[col] = rescale(combined[col])
}
  
#split data sets
traindf = combined[1:1460,]
testdf = combined[1461:2919,]  









