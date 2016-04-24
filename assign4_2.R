#######################################################################################################
PROBLEM STATEMENT:

Choose the two models from Exercise 1 that gave the lowest CV error. For each model, use the
bootstrap method to estimate the 95% error-confidence interval. Use b = 100; i.e. construct
100 different bootstrap samples. For each model, list all 100 model errors. Based on the two
confidence intervals, which model would you select for deployment?
#####################################################################################################

ques2=function(dataset,svm)
{
  result=c()
  for(i in 1:100)
  {
    sampledata=dataset[sample(nrow(dataset),replace=T),]
    error=table(predict(svm,subset(sampledata,select=-Severity)),sampledata$Severity)
    error2=(error[1,2]+error[2,1])/length(dataset$Severity)*100
    cat("ERROR ",i,":",error2,"\n")
    #print(percent)
    result=c(result,error2)
  }
  result=sort(result)
  #print(result)
  lower=(result[2]+result[3])/2
  upper=(result[97]+result[98])/2
  print("Lower bound")
  print(lower)
  print("Upper bound")
  print(upper)
}
