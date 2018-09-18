knn <- function(data,k=5) #default k value for the function is 5
  #data input for the function would be combination of train and test data
  {
  #using complete.cases to get train data
  data_complete <- data[complete.cases(data),] 
  #splitting the dataset to test data
  data_incomplete <- data[!complete.cases(data),] 
  #calculating the number of rows for train data
  ncomplete <- length(data_complete[,1]) 
  #calculating the number of rows for test data
  nincomplete <- length(data_incomplete[,1]) 
  #iterating over the test dataset in order to calculate the nearest neighbour
  for(j in 1:nincomplete) {
    #combining euclidean distance and the training data
    data_complete <- cbind(data_complete,euc.distance(data_complete, data_incomplete[j,]))
    #renaming column for distance to dist
    colnames(data_complete)[ncol(data_complete)]<-paste('dist')
    #sorting dataset by distance
    data_complete <- data_complete[order(data_complete$dist),]
    #Invere squaring distance
    data_complete$indist <- 1/ (data_complete$dist)^2
    #multiplying inverse squared distance with saleprice
    data_complete$sale_dist <- data_complete$ytrain * data_complete$indist
    #calculating the predicted sale price for jth element in test data using nearest neighbor logic (inverse squared weighted average)
    data_incomplete$ytrain[j] <- sum(as.data.frame(head(data_complete$sale_dist,k)))/ sum(as.data.frame(head(data_complete$indist,k)))
    #dropping temporary tables
    data_complete$dist <-  NULL
    data_complete$indist <-  NULL
    data_complete$sale_dist <-  NULL
  }
return(data_incomplete)
}

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
