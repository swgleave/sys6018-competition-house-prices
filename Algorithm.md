Rationale behind choice of statistical Method - KNN

 As there are a large number of features in the model, it makes most sense to evaluate the model using simarity of 
 a point in the test dataset with points from the train dataset. KNN is easily one of the best choices for this use case. 
 We implemented the KNN from scratch using the following steps.
 
 1. Calculate Euclidean distance between each point in the test dataset with train dataset - Accomplished this using matrix 
 operations and apply functions
 2. Based on the K value, pick the nearest neighbors based on the distance calculations.
 3. Use a inverse squared weighted average to calculate the predicted value for the test point.
 
 Y(pred) = Sum(Yi/(Di)62)/Sum(1/Di^2) where i runs from 1 to k 
 
 
 
