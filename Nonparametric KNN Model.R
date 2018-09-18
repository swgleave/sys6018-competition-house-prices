library(tidyverse)
library(purrr)
library(ggplot2)
library(zoo)
library(onehot)
library(FNN)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
SalePrice <- train$SalePrice
train <- train[,c(1:80)]
all <- rbind(train,test)

collist <- names(all)
indlist <- seq(1,80,by=1)
collist <- cbind(collist,indlist) # Create a list of column names and their indexes

####################
####################
#  Address NAs       
#  
####################
####################

all$ExterCond <- as.character(all$ExterCond)
all$ExterQual <- as.character(all$ExterQual)

all$ExterQual[which(all$ExterQual == "Ex")] <- 5
all$ExterQual[which(all$ExterQual == "Fa")] <- 2
all$ExterQual[which(all$ExterQual == "Gd")] <- 4
all$ExterQual[which(all$ExterQual == "TA")] <- 3
all$ExterQual[which(all$ExterQual == "Po")] <- 1

all$ExterCond[which(all$ExterCond == "Ex")] <- 5
all$ExterCond[which(all$ExterCond == "Fa")] <- 2
all$ExterCond[which(all$ExterCond == "Gd")] <- 4
all$ExterCond[which(all$ExterCond == "TA")] <- 3
all$ExterCond[which(all$ExterCond == "Po")] <- 1

all$ExterCond <- as.numeric(all$ExterCond)
all$ExterQual <- as.numeric(all$ExterQual)

table(all$MSZoning)
all$MSZoning[which(is.na(all$MSZoning))] <- "RL"

levels(all$Alley) <- c(levels(all$Alley),"Not Applicable")    # Add None level
all$Alley[which(is.na(all$Alley))] <- "Not Applicable"        # Set NAs to None

all$Utilities[which(is.na(all$Utilities))] <- "AllPub"     # Set Utilities NA to AllPub

all$Exterior1st[which(is.na(all$Exterior1st))] <- "VinylSd"   # Guess Vinyl Siding for NA
all$Exterior2nd[which(is.na(all$Exterior2nd))] <- "VinylSd"   # Guess Vinyl Siding for NA

all$MasVnrType[which(is.na(all$MasVnrType))] <- "None"    # Set NAs to None
all$MasVnrArea[which(is.na(all$MasVnrArea))] <- 0         # Set NAs to 0

levels(all$BsmtCond) <- c(levels(all$BsmtCond),"None")    # Add None level
all$BsmtCond[which(is.na(all$BsmtCond))] <- "None"        # Set NAs to None

levels(all$BsmtQual) <- c(levels(all$BsmtQual),"None")
all$BsmtQual[which(is.na(all$BsmtQual))] <- "None"

all$BsmtExposure[which(is.na(all$BsmtExposure) & !is.na(all$BsmtFinType1))] <- "No"      # Set unique NA to No

levels(all$BsmtExposure) <- c(levels(all$BsmtExposure),"None")
all$BsmtExposure[which(is.na(all$BsmtExposure))] <- "None"

all$BsmtFinType2[which(is.na(all$BsmtFinType2) & !is.na(all$BsmtFinType1))] <- "Unf"     # Set unique NA to Unf

levels(all$BsmtFinType2) <- c(levels(all$BsmtFinType2),"None")
all$BsmtFinType2[which(is.na(all$BsmtFinType2))] <- "None"

levels(all$BsmtFinType1) <- c(levels(all$BsmtFinType1),"None")
all$BsmtFinType1[which(is.na(all$BsmtFinType1))] <- "None"

all$Electrical[which(is.na(all$Electrical))] <- "SBrkr"    # Set one NA to SBrkr (most common value)

all$KitchenQual[which(is.na(all$KitchenQual))] <- "TA"
all$Functional[which(is.na(all$Functional))] <- "Typ"

levels(all$FireplaceQu) <- c(levels(all$FireplaceQu),"None")
all$FireplaceQu[which(is.na(all$FireplaceQu))] <- "None"

levels(all$PoolQC) <- c(levels(all$PoolQC),"None")
all$PoolQC[which(is.na(all$PoolQC))] <- "None"

levels(all$GarageType) <- c(levels(all$GarageType),"None")
all$GarageType[which(is.na(all$GarageType))] <- "None"

levels(all$GarageFinish) <- c(levels(all$GarageFinish),"None")
all$GarageFinish[which(is.na(all$GarageFinish))] <- "None"

levels(all$GarageQual) <- c(levels(all$GarageQual),"None")
all$GarageQual[which(is.na(all$GarageQual))] <- "None"

levels(all$GarageCond) <- c(levels(all$GarageCond),"None")
all$GarageCond[which(is.na(all$GarageCond))] <- "None"

levels(all$Fence) <- c(levels(all$Fence),"None")
all$Fence[which(is.na(all$Fence))] <- "None"

levels(all$MiscFeature) <- c(levels(all$MiscFeature),"None")
all$MiscFeature[which(is.na(all$MiscFeature))] <- "None"

all$LotFrontage[which(is.na(all$LotFrontage))] <- 0

all$SaleType[which(is.na(all$SaleType))] <- "WD"

all$BsmtFinSF1[which(is.na(all$BsmtFinSF1))] <- 0
all$BsmtFinSF2[which(is.na(all$BsmtFinSF2))] <- 0
all$BsmtUnfSF[which(is.na(all$BsmtUnfSF))] <- 0
all$TotalBsmtSF[which(is.na(all$TotalBsmtSF))] <- 0

all$BsmtFullBath[which(is.na(all$BsmtFullBath))] <- 0
all$BsmtHalfBath[which(is.na(all$BsmtHalfBath))] <- 0

all$GarageYrBlt[which(is.na(all$GarageYrBlt))] <- floor(mean(all$GarageYrBlt[which(!is.na(all$GarageYrBlt))]))
all$GarageYrBlt[which(all$GarageYrBlt > 2010)] <- 2007

all$GarageCars[which(is.na(all$GarageCars))] <- 0
all$GarageArea[which(is.na(all$GarageArea))] <- 0
all$RoofMatl[which(all$RoofMatl == "ClyTile")] <- "Tar&Grv"
all$RoofMatl <- factor(all$RoofMatl)


allCat <- all[,c(2,3,6:17,22:26,30:34,36,40:43,54,56,58:59,61,64:66,73:75,79:80)] # Subset Categorical
allCon <- all[,c(4:5,18:21,27:29,35,37:39,44:53,55,57,60,62,63,67:72,76:78)] # Subset Quantitative

lapply(allCat,class)
allCat <- lapply(allCat,as.factor)
lapply(allCat,class)
str(allCat) # Turn Categorical Subset into all Factors

all2 <- cbind(allCat,allCon) # Recreate train with Categorical and Quantitative

train2 <- all2[c(1:1460),]
test2 <- all2[c(1461:2919),]

train2 <- cbind(train2,SalePrice)

####################
####################
#  Remove Outliers       
#  
####################
####################

hist(train2$SalePrice, breaks = 200)   # Histogram Sale Price
tail(sort(train2$SalePrice),30)   # Highest 30 Sale Prices

rmtrain2 <- train2[which(train2$SalePrice > mean(train2$SalePrice) + 3 * sd(train2$SalePrice)),] # Remove 22 values outside of 3 Standard Deviations
train2 <- train2[which(train2$SalePrice < mean(train2$SalePrice) + 3 * sd(train2$SalePrice)),] # Update with only values within 3 Standard Deviations

rmtrain2 <- rbind(rmtrain2,train2[which(train2$LotArea > (mean(train2$LotArea) + 3 * sd(train2$LotArea))),]) # Remove 11 rows that have Lot Area outside 3 standard deviations
train2 <- train2[which(train2$LotArea < (mean(train2$LotArea) + 3 * sd(train2$LotArea))),]


####################
####################
#  One Hot Encode       
#  Factors
####################
####################

ytrain = train2$SalePrice

traindummies = onehot(train2, stringsAsFactors = FALSE, addNA = FALSE, max_levels = 100)
traindummies[length(traindummies)] <- NULL #removed sale price column so test and train are equal


trainmatrix <- predict(traindummies, train2)
testmatrix <- predict(traindummies, test2)
traindf = as.data.frame(trainmatrix)
testdf = as.data.frame(testmatrix)

#make negatives zero
traindf[traindf < 0] <- 0 
testdf[testdf < 0 ] <- 0


####################
####################
#  Normalize       
#  Numeric
####################
####################

#concatenate test and train data so normalized over same data
combined = rbind(traindf, testdf)

rescale <- function(x){(x-min(x))/(max(x)-min(x))}

cols2 = colnames(combined)
for (col in cols2){
  combined[col] = rescale(combined[col])
}

#split data sets
traindf = combined[1:1426,]
testdf = combined[1425:2885,]  


####################
####################
#  Validate        
#  
####################
####################

library(MLmetrics)
#split up train for testing
trainsplit<- sample(1:1426, size=713) 
trainset <- traindf[trainsplit,]
validset <- traindf[-trainsplit,]
ytrain <- as.data.frame(ytrain)
yset <- ytrain[trainsplit,]
yvalidset <- ytrain[-trainsplit,]

######### Calc Euclidean distance function #####

euc.distance <- function(d1, d2){
  # return euclidean distance
  # takes two argument
  # example distance(df1, df2)
  # d1 is train dataframe
  # d2 is a test datapoint
  # returns a vector with all the distance of a test data point from each train data point
  d1 = d1[,1:(ncol(d1)-1)]
  d2 = d2[,1:(ncol(d2)-1)]
  train.row <- nrow(d1)
  dis <- sapply(apply((apply(data.frame(matrix(rep(d2,each=train.row),nrow=train.row)),2,as.numeric) - d1)^2,1,sum),sqrt)
  dis
  return(dis)
}

######### Define our KNN function #####

knn <- function(data,k=5) 
{
  data_complete <- data[complete.cases(data),]
  data_incomplete <- data[!complete.cases(data),]
  ncomplete <- length(data_complete[,1])
  nincomplete <- length(data_incomplete[,1])
  for(j in 1:nincomplete) {
    data_complete <- cbind(data_complete,euc.distance(data_complete, data_incomplete[j,]))
    colnames(data_complete)[ncol(data_complete)]<-paste('dist')
    sum(data_complete$dist)
    data_complete <- data_complete[order(data_complete$dist),]
    data_complete$indist <- 1/ (data_complete$dist)^2
    data_complete$sale_dist <- data_complete$ytrain * data_complete$indist
    data_incomplete$ytrain[j] <- sum(as.data.frame(head(data_complete$sale_dist,k)))/ sum(as.data.frame(head(data_complete$indist,k)))
    data_complete$dist <-  NULL
    data_complete$indist <-  NULL
    data_complete$sale_dist <-  NULL
  }
  return(data_incomplete)
}

trainset$ytrain <- yset   
validset$ytrain <- NA

combinedvalid <- rbind(trainset,validset)
prediction=knn(combinedvalid, k=5)

RMSLE(prediction$ytrain, yvalidset)
# 0.1923941 


####################
####################
#  Test Predictions       
#  
####################
####################

testdf$ytrain = NA
traindf$ytrain <- train2$SalePrice

combinedpred = rbind(traindf, testdf)

prediction=knn(combinedpred, k=3)

results = cbind(test$Id, prediction$ytrain)
results = as.data.frame(results)

colnames(results) <- c('Id', 'SalePrice')

write.csv(results, 'submission.csv', row.names = FALSE)

