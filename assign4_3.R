ques3=function(dataset)
{
  
model1=svm(dataset$Severity~.,dataset,kernel="polynomial",degree=2,cost=1000,type="C-classification",cross=10)
model2=svm(dataset$Severity~.,dataset,kernel="polynomial",degree=3,cost=10000,type="C-classification",cross=10)
conf1=table(fitted(model1),dataset$Severity)
conf2=table(fitted(model2),dataset$Severity)
print("CONFUSION MATRIX 1 FOR (c,N)=(1000,2)")
print(conf1)
print("CONFUSION MATRIX 2 FOR (c,N)=(10000,3)")
print(conf2)

}