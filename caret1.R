SonarExample<-function(){
  library(caret)
  library(mlbench)
  data("Sonar")
  set.seed(10)
  Train<-createDataPartition(y=Sonar$Class,p = 0.75, list = FALSE)
  str(Train)
  trainingSet<-Sonar[Train,]
  testingSet<-Sonar[-Train,]
  nrow(trainingSet)
  nrow(testingSet)
  ctrl <- trainControl(method = "repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)  
  plsFit<-train(Class ~.,data = trainingSet, method="pls",tuneLength=15,trControl=ctrl,metric="ROC",preProc = c("center", "scale"))
  print(plsFit)
  plsClasses <- predict(plsFit, newdata = testingSet)
  plsProbs <- predict(plsFit, newdata = testingSet, type = "prob")
  plot(plsFit)
  confusionMatrix(data = plsClasses, testingSet$Class)
}