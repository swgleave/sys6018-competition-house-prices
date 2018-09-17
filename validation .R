#split up train for testing
trainsplit<- sample(1:1424, size=712) 
trainset <- traindf[trainsplit,]
validset <- traindf[-trainsplit,]
ytrain <- as.data.frame(ytrain)
yset <- ytrain[trainsplit,]
yvalidset <- c(ytrain[-trainsplit,])

#test on knn model
model1<- knn.reg(train=trainset, test=validset, yset, k=5)

resultsmatrixtrue <- c(model1$pred)


RMSLE(resultsmatrixtrue, yvalidset)