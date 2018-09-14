library(tidyverse)
library(purrr)
library(ggplot2)

train <- read.csv("train.csv")

hist(train$SalePrice, breaks = 200)   # Histogram Sale Price
tail(sort(train$SalePrice),30)   # Highest 30 Sale Prices
mean(train$SalePrice) + 3 * sd(train$SalePrice)  # Mean + 3 Standard Deviations = 419249
rmtrain <- train[which(train$SalePrice > 419249),] # Remove 22 values outside of 3 Standard Deviations
train <- train[which(train$SalePrice < 419249),] # Update with only values within 3 Standard Deviations
hist(train$SalePrice, breaks = 200) # Histogram Sale Price (updated)

collist <- names(train)
indlist <- seq(1,81,by=1)
collist <- cbind(collist,indlist) # Create a list of column names and their indexes

trainCat <- train[,c(2,3,6:19,22:26,28:34,36,40:43,48:59,61,62,64:66,73:75,77,79:80)] # Subset Categorical
trainCon <- train[,c(1,4:5,20:21,27,35,37:39,44:47,60,63,67:72,76,78,81)] # Subset Quantitative

lapply(trainCat,class)
trainCat <- lapply(trainCat,as.factor)
lapply(trainCat,class)
str(trainCat) # Turn Categorical Subset into all Factors

train2 <- cbind(trainCat,trainCon) # Recreate train with Categorical and Quantitative
str(train2)
levels(train2$Alley) <- c(levels(train2$Alley),"None") # Add None level
train2$Alley[which(is.na(train2$Alley))] <- "None"     # Change NAs to None

train2$LotFrontage[which(is.na(train2$LotFrontage))] <- 0       # Set NAs to 0
train2$MasVnrType[which(is.na(train2$MasVnrType))] <- "None"    # Set NAs to None
train2$MasVnrArea[which(is.na(train2$MasVnrArea))] <- 0         # Set NAs to 0
levels(train2$BsmtCond) <- c(levels(train2$BsmtCond),"None")    # Add None level
train2$BsmtCond[which(is.na(train2$BsmtCond))] <- "None"        # Set NAs to None

levels(train2$BsmtQual) <- c(levels(train2$BsmtQual),"None")
train2$BsmtQual[which(is.na(train2$BsmtQual))] <- "None"

train2$BsmtExposure[which(is.na(train2$BsmtExposure) & !is.na(train2$BsmtFinType1))] <- "No"      # Set unique NA to No
levels(train2$BsmtExposure) <- c(levels(train2$BsmtExposure),"None")
train2$BsmtExposure[which(is.na(train2$BsmtExposure))] <- "None"

train2$BsmtFinType2[which(is.na(train2$BsmtFinType2) & !is.na(train2$BsmtFinType1))] <- "Unf"     # Set unique NA to Unf
levels(train2$BsmtFinType2) <- c(levels(train2$BsmtFinType2),"None")
train2$BsmtFinType2[which(is.na(train2$BsmtFinType2))] <- "None"

levels(train2$BsmtFinType1) <- c(levels(train2$BsmtFinType1),"None")
train2$BsmtFinType1[which(is.na(train2$BsmtFinType1))] <- "None"

train2$Electrical[which(is.na(train2$Electrical))] <- "SBrkr"    # Set one NA to SBrkr (most common value)
table(train2$Electrical)

levels(train2$FireplaceQu) <- c(levels(train2$FireplaceQu),"None")
train2$FireplaceQu[which(is.na(train2$FireplaceQu))] <- "None"

levels(train2$GarageType) <- c(levels(train2$GarageType),"None")
train2$GarageType[which(is.na(train2$GarageType))] <- "None"

levels(train2$GarageFinish) <- c(levels(train2$GarageFinish),"None")
train2$GarageFinish[which(is.na(train2$GarageFinish))] <- "None"

levels(train2$GarageQual) <- c(levels(train2$GarageQual),"None")
train2$GarageQual[which(is.na(train2$GarageQual))] <- "None"

levels(train2$GarageCond) <- c(levels(train2$GarageCond),"None")
train2$GarageCond[which(is.na(train2$GarageCond))] <- "None"

levels(train2$PoolQC) <- c(levels(train2$PoolQC),"None")
train2$PoolQC[which(is.na(train2$PoolQC))] <- "None"

levels(train2$Fence) <- c(levels(train2$Fence),"None")
train2$Fence[which(is.na(train2$Fence))] <- "None"

levels(train2$MiscFeature) <- c(levels(train2$MiscFeature),"None")
train2$MiscFeature[which(is.na(train2$MiscFeature))] <- "None"

rmtrain2 <- train2[which(train$LotArea > (mean(train$LotArea) + 3 * sd(train$LotArea))),] # Remove 12 rows that have Lot Area outside 3 standard deviations
train2 <- train2[which(train$LotArea < (mean(train$LotArea) + 3 * sd(train$LotArea))),]

test <- read.csv("test.csv")

collist2 <- names(test)
indlist2 <- seq(1,80,by=1)
collist2 <- cbind(collist2,indlist2) # Create a list of column names and their indexes

testCat <- test[,c(2,3,6:19,22:26,28:34,36,40:43,48:59,61,62,64:66,73:75,77,79:80)] # Subset Categorical
testCon <- test[,c(1,4:5,20:21,27,35,37:39,44:47,60,63,67:72,76,78)] # Subset Quantitative

lapply(testCat,class)
testCat <- lapply(testCat,as.factor)
lapply(testCat,class)
str(testCat) # Turn Categorical Subset into all Factors

test2 <- cbind(testCat,testCon) # Recreate test with Categorical and Quantitative

levels(test2$Alley) <- c(levels(test2$Alley),"None") # Add None level
test2$Alley[which(is.na(test2$Alley))] <- "None"     # Change NAs to None

test2$LotFrontage[which(is.na(test2$LotFrontage))] <- 0       # Set NAs to 0
test2$MasVnrType[which(is.na(test2$MasVnrType))] <- "None"    # Set NAs to None
test2$MasVnrArea[which(is.na(test2$MasVnrArea))] <- 0         # Set NAs to 0
levels(test2$BsmtCond) <- c(levels(test2$BsmtCond),"None")    # Add None level
test2$BsmtCond[which(is.na(test2$BsmtCond))] <- "None"        # Set NAs to None

levels(test2$BsmtQual) <- c(levels(test2$BsmtQual),"None")
test2$BsmtQual[which(is.na(test2$BsmtQual))] <- "None"

test2$BsmtExposure[which(is.na(test2$BsmtExposure) & !is.na(test2$BsmtFinType1))] <- "No"      # Set unique NA to No
levels(test2$BsmtExposure) <- c(levels(test2$BsmtExposure),"None")
test2$BsmtExposure[which(is.na(test2$BsmtExposure))] <- "None"

test2$BsmtFinType2[which(is.na(test2$BsmtFinType2) & !is.na(test2$BsmtFinType1))] <- "Unf"     # Set unique NA to Unf
levels(test2$BsmtFinType2) <- c(levels(test2$BsmtFinType2),"None")
test2$BsmtFinType2[which(is.na(test2$BsmtFinType2))] <- "None"

levels(test2$BsmtFinType1) <- c(levels(test2$BsmtFinType1),"None")
test2$BsmtFinType1[which(is.na(test2$BsmtFinType1))] <- "None"

test2$Electrical[which(is.na(test2$Electrical))] <- "SBrkr"    # Set one NA to SBrkr (most common value)
table(test2$Electrical)

levels(test2$FireplaceQu) <- c(levels(test2$FireplaceQu),"None")
test2$FireplaceQu[which(is.na(test2$FireplaceQu))] <- "None"

levels(test2$GarageType) <- c(levels(test2$GarageType),"None")
test2$GarageType[which(is.na(test2$GarageType))] <- "None"

levels(test2$GarageFinish) <- c(levels(test2$GarageFinish),"None")
test2$GarageFinish[which(is.na(test2$GarageFinish))] <- "None"

levels(test2$GarageQual) <- c(levels(test2$GarageQual),"None")
test2$GarageQual[which(is.na(test2$GarageQual))] <- "None"

levels(test2$GarageCond) <- c(levels(test2$GarageCond),"None")
test2$GarageCond[which(is.na(test2$GarageCond))] <- "None"

levels(test2$PoolQC) <- c(levels(test2$PoolQC),"None")
test2$PoolQC[which(is.na(test2$PoolQC))] <- "None"

levels(test2$Fence) <- c(levels(test2$Fence),"None")
test2$Fence[which(is.na(test2$Fence))] <- "None"

levels(test2$MiscFeature) <- c(levels(test2$MiscFeature),"None")
test2$MiscFeature[which(is.na(test2$MiscFeature))] <- "None"


#Start of Will's code
ytrain = train2$SalePrice


#one hot encoding

traindummies = onehot(train2, stringsAsFactors = FALSE, addNA = FALSE, max_levels = 100)
traindummies[length(traindummies)] <- NULL #removed sale price column so test and train are equal

trainmatrix<- predict(traindummies, train2)
testmatrix<- predict(traindummies, test2)

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
traindf = combined[1:1426,]
testdf = combined[1427:2885,]  


#test on knn model
model1<- knn.reg(train=traindf, test=testdf, ytrain, k=3)

results = cbind(test2$Id, model1$pred)
results = as.data.frame(results)

colnames(results) <- c('Id', 'SalePrice')

write.csv(results, 'submission.csv', row.names = FALSE)
