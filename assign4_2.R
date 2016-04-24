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