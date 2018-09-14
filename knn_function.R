knn <- function(data, imp_col, train_cols, k=5) {

  data_complete <- data[complete.cases(data),]
  data_incomplete <- data[!complete.cases(data),]
  ncomplete <- length(data_complete[,1])
  nincomplete <- length(data_incomplete[,1])
  
  for(j in 1:nincomplete) {
    d <- numeric(ncomplete)
    for(i in train_cols) 
      d <- d + (data_complete[,i] - data_incomplete[j,i])^2
    # indices of k-nearest neighbors
    knn_index <- head(sort(d,index.return=T)$ix,k) 
    nn <- sort(table(data_complete[knn_index,imp_col]), T)
    data_incomplete[j,imp_col]<- names(nn)[1]
  }
  data_incomplete
}


data_incomplete
}
