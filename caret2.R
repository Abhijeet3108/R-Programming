caret2<-function(leaf){
  library(caret)
  leaf = read.csv("leaf.csv", colClasses = c(Class = "factor"))
  
  ctrl = trainControl(method="repeatedcv", number=10, repeats=5, selectionFunction = "oneSE")
  Train = createDataPartition(leaf$Class, p=.75, list=FALSE)
  test = leaf[-in_train,]
  
  trf = train(Class ~.,data=leaf, method="rf", metric="Kappa",trControl=ctrl, subset = Train)
  print(trf)
  
  tgbm = train(Class ~., data=leaf, method="gbm", metric="Kappa",trControl=ctrl, subset = Train, verbose=FALSE)
  print(tgbm)
  
  compare = resamples(list(RF = trf,GBM = tgbm))
  difValues = diff(compare)
  print(summary(difValues))
  test$pred.leaf.rf = predict(trf, test, "raw")
  confusionMatrix(test$pred.leaf.rf, test$Class)
  varImp(trf, scale=FALSE)
}