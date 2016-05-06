Problem Statement:
The leaf dataset downloaded form UCI machine learning repository comprises of leaves from almost 40 different plant species, 
and has 14 numerical attributes describing each leaf. Aim of the R-script is to predict to which species they belong using two
strategies namely, Random Forests(rf) and Gradient Boosted Machine(gbm). Compare the accuracy of classification of the two methods 
and hence derive a reasoning as to which one is better. Moreover, find out the order of attributes that proved to be most valuable
in the classification and sort them in increasing order of importance.
#######################################################################################################################################

Solution:

caret2<-function(leaf){
  library(caret)  #import the caret library
  
  leaf = read.csv("leaf.csv", colClasses = c(Class = "factor")) #reading the csv file and storing in vector leaf
  
  ctrl = trainControl(method="repeatedcv", number=10, repeats=5, selectionFunction = "oneSE")
  #Controls the computational parameters of the train function such as selection function, method etc.
  
  Train = createDataPartition(leaf$Class, p=.75, list=FALSE)
   #Creating a data partition of the 208 total observations, so that 75% are used for training 
  #(as indicated by p-value, and the rest 25% for testing purposes.
  
  test = leaf[-in_train,] # Vector test stores 25% of the observations for just testing purposes. 
  
  ###For Random Forests#####
  trf = train(Class ~.,data=leaf, method="rf", metric="Kappa",trControl=ctrl, subset = Train)
  #Train the data based on the training dataset with other parameters mentioned such as tuneLength, preprocessing 
  #technique, Repeated Cross-Validation etc.
  print(trf) #Print the summary of Random forests
  
  
  ###For Gradient Boosting Machines#####
  tgbm = train(Class ~., data=leaf, method="gbm", metric="Kappa",trControl=ctrl, subset = Train, verbose=FALSE)
  print(tgbm)
   #Similarly, train the data using the Gradient boosting machines.
   
  compare = resamples(list(RF = trf,GBM = tgbm))  #Comparison betwwen the trf and tgbm
  difValues = diff(compare) 
  print(summary(difValues)) #Print the summary of comparison
  
  test$pred.leaf.rf = predict(trf, test, "raw") #Based on the above comparison the best one is chosen which happens to be the RF.
  confusionMatrix(test$pred.leaf.rf, test$Class)  #Print out the confusion Matrix
  varImp(trf, scale=FALSE)  
  #calculate variable importance so that we can see which variables were the most informative in making distinctions between classes.
}


################OUTPUT########################

1)FOR Random Forest:

Random Forest 

340 samples
 15 predictor
 30 classes: '1', '10', '11', '12', '13', '14', '15', '2', '22', '23', '24', '25', '26', '27', '28', '29', '3', '30', '31', '32', '33', '34', '35', '36', '4', '5', '6', '7', '8', '9' 

No pre-processing
Resampling: Cross-Validated (10 fold, repeated 5 times) 
Summary of sample sizes: 236, 240, 236, 236, 242, 239, ... 
Resampling results across tuning parameters:

  mtry  Accuracy   Kappa    
   2    0.7716966  0.7627624
   8    0.7876670  0.7793798
  15    0.7678328  0.7587987

Kappa was used to select the optimal model using  the one SE rule.
The final value used for the model was mtry = 8. 


2)For Stochastic Gradient Boosting:
Stochastic Gradient Boosting 

340 samples
 15 predictor
 30 classes: '1', '10', '11', '12', '13', '14', '15', '2', '22', '23', '24', '25', '26', '27', '28', '29', '3', '30', '31', '32', '33', '34', '35', '36', '4', '5', '6', '7', '8', '9' 

No pre-processing
Resampling: Cross-Validated (10 fold, repeated 5 times) 
Summary of sample sizes: 236, 234, 239, 239, 238, 241, ... 
Resampling results across tuning parameters:

  interaction.depth  n.trees  Accuracy   Kappa    
  1                   50      0.6560060  0.6429532
  1                  100      0.6678559  0.6551057
  1                  150      0.6659896  0.6532278
  2                   50      0.6982685  0.6865076
  2                  100      0.7121462  0.7010206
  2                  150      0.7097964  0.6986062
  3                   50      0.6939676  0.6822479
  3                  100      0.7093912  0.6981678
  3                  150      0.6988477  0.6872266

Tuning parameter 'shrinkage' was held constant at a value of 0.1
Tuning parameter 'n.minobsinnode' was held constant at
 a value of 10
Kappa was used to select the optimal model using  the one SE rule.
The final values used for the model were n.trees = 100, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10. 

Call:
summary.diff.resamples(object = difValues)

p-value adjustment: bonferroni 
Upper diagonal: estimates of the difference
Lower diagonal: p-value for H0: difference = 0

Accuracy 
    RF        GBM    
RF            0.07552
GBM 1.846e-05        

Kappa 
    RF        GBM    
RF            0.07836
GBM 1.812e-05        

rf variable importance

              Overall
Solidity       39.314
Aspect         24.128
Elongation     23.188
Eccentricity   21.621
Isoperimetric  21.194
Entropy        20.338
Stochastic     15.000
Depth          14.535
Lobedness      14.040
Uniformity     13.952
Moment         13.285
Intensity      11.788
Contrast       10.098
Smoothness      9.890
Specimen        2.661
