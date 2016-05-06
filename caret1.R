Problem Statement:
The Sonar data set is used by Gorman and Sejnowski in their study of the classification of sonar signals using a neural network. 
The task is to train a network to discriminate between sonar signals bounced off a metal cylinder and those bounced off a roughly
cylindrical rock. Each pattern is a set of 60 numbers in the range 0.0 to 1.0. Each number represents the energy within a particular
frequency band, integrated over a certain period of time. The integration aperture for higher frequencies occur later in time, since
these frequencies are transmitted later during the chirp. The label associated with each record contains the letter "R" if the object
is a rock and "M" if it is a mine (metal cylinder). The numbers in the labels are in increasing order of aspect angle, but they do not
encode the angle directly.
###################################################################################################################################
Solution:


SonarExample<-function(){
  library(caret)    #Loading the caret library
  library(mlbench)  #loading the mlbench library 
  data("Sonar")     #Sonar dataset is provided by the mlbench library
  set.seed(10)      #Setting the seed value to 10
  
  ##########DATA PARTITIONING##############
  Train<-createDataPartition(y=Sonar$Class,p = 0.75, list = FALSE)
  #Creating a data partition of the 208 total observations, so that 75% are used for training (as indicated by p-value, and the rest
  #25% for testing purposes.
  
  str(Train)    #structure of the train object
  trainingSet<-Sonar[Train,]  #The training set of 157 observations
  testingSet<-Sonar[-Train,]  #The testing set of remaining 51 observations
  nrow(trainingSet)
  nrow(testingSet)
  
  ##########DATA TRAINING####################
  ctrl <- trainControl(method = "repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
  #Controls the computational parameters of the train function such as the summary function, method etc.
  plsFit<-train(Class ~.,data = trainingSet, method="pls",tuneLength=15,trControl=ctrl,metric="ROC",preProc = c("center", "scale"))
  #Train the data based on the training dataset with other parameters mentioned such as tuneLength, preprocessing technique, ROC etc.
  print(plsFit)
  
  ###############DATA PREDICTION###############
  plsClasses <- predict(plsFit, newdata = testingSet) ) #Predict the classes of the testing dataset
  plsProbs <- predict(plsFit, newdata = testingSet, type = "prob")  #Compute the class probabilites
  plot(plsFit)  #Plot the model
  confusionMatrix(data = plsClasses, testingSet$Class)  #Print the confusion matrix
}



######################OUTPUT##############################

 int [1:157, 1] 2 3 4 5 6 7 10 12 13 14 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr "Resample1"
Partial Least Squares 

157 samples
 60 predictor
  2 classes: 'M', 'R' 

Pre-processing: centered (60), scaled (60) 
Resampling: Cross-Validated (10 fold, repeated 3 times) 
Summary of sample sizes: 141, 141, 141, 142, 142, 141, ... 
Resampling results across tuning parameters:

  ncomp  ROC        Sens       Spec     
   1     0.7921958  0.6930556  0.7101190
   2     0.8481399  0.7652778  0.8011905
   3     0.8371445  0.7796296  0.7684524
   4     0.8449239  0.7712963  0.7601190
   5     0.8398231  0.7791667  0.7684524
   6     0.8256035  0.7842593  0.7226190
   7     0.8093419  0.7671296  0.6904762
   8     0.8176835  0.7828704  0.7226190
   9     0.8189401  0.7824074  0.7351190
  10     0.8283399  0.7787037  0.7529762
  11     0.8275546  0.7870370  0.7577381
  12     0.8164931  0.8032407  0.7535714
  13     0.8079944  0.7875000  0.7309524
  14     0.7992973  0.7953704  0.7386905
  15     0.7948247  0.7875000  0.7107143

ROC was used to select the optimal model using  the largest value.
The final value used for the model was ncomp = 2. 
Confusion Matrix and Statistics

          Reference
Prediction  M  R
         M 23  6
         R  4 18
                                          
               Accuracy : 0.8039          
                 95% CI : (0.6688, 0.9018)
    No Information Rate : 0.5294          
    P-Value [Acc > NIR] : 4.341e-05       
                                          
                  Kappa : 0.6047          
 Mcnemar's Test P-Value : 0.7518          
                                          
            Sensitivity : 0.8519          
            Specificity : 0.7500          
         Pos Pred Value : 0.7931          
         Neg Pred Value : 0.8182          
             Prevalence : 0.5294          
         Detection Rate : 0.4510          
   Detection Prevalence : 0.5686          
      Balanced Accuracy : 0.8009          
                                          
       'Positive' Class : M         
       
To conclude : An accuracy of 80.39% was achieved while classifying the data. 
