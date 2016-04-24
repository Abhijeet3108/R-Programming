#########################################################################################################3
PROBLEM STATEMENT:

Review and download the mammographic mass data set at https://archive.ics.uci.edu/ml/datasets/Mammographic+Mass
Using R, load the data set into a data frame a create 4 × 6 = 24 different svm models of this
data, where each model can be characterized as an element from the set
{(n, C)|1 ≤ n ≤ 4 and C = 0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0}.
1
Here n represents the degree of a polynomial classifier (note: for n = 1, use a linear classifier),
and C represents the slack cost for a soft-margin classifier. Provide a table that, for each
model, provides its model error (when trained and tested on the entire data set) and its 10-fold
cross-validation error.
#######################################################################################################

ques1<-function(dataset)
{
	k=1
	cost1=c(0.1,1.0,10.0,100.0,1000.0,10000.0) # given cost vector
	models=vector("list",25)#empty array to store the 24 combinations of models that will be generated
	models2=vector("list",25)
	accuracy=c()
	accuracy1=c()
	error=c()
	i=1
	j=1
	m=1
	degree=c()
	cost=c()
	
	# FOR CALCULATION OF 10-FOLD CROSS VALIDATION ERROR
	for(i in 1:6) #for each of the six values in cost vector
	{
		for(j in 1:4) #for degree ranging from 1 through 4
		{
			if(j==1) #for linear models
			{
				temp=svm(dataset$Severity~.,data=dataset,kernel="linear",cost=cost1[i],cross=10,type="C-classification") 
				#create an svm with 10-fold cross validation and linear kernel  
				accuracy1[k]=summary(temp)$tot.accuracy #store the accuracy in a vector
				error[k]=100-accuracy1[k] # calculate error i.e. 100-accuracy
				models[[k]]=temp
				k=k+1
			}
			else #for degree greater than 1
			{
				temp=svm(dataset$Severity~.,data=dataset,kernel="polynomial",cost=cost1[i],cross=10,type="C-classification",degree=j)
				#create an svm with 10-fold cross validation, polynomial kernel and degree as per the current iteration of j
				accuracy1[k]=summary(temp)$tot.accuracy #store the accuracy in a vector
				error[k]=100-accuracy1[k] #calculate error i.e. 100-accuracy
        models[[k]]=temp
				k=k+1

			}
		}
	}
	
	# FOR CALCULATION OF MODEL ERROR
	k=1
	for(i in 1:6) #for each of the six values in cost vector
	{
	  for(j in 1:4) #for degree ranging from 1 through 4
	  {
	    if(j==1) #for linear models
	    {
	      models2[[k]]=svm(dataset$Severity~.,data=dataset,kernel="linear",cost=cost1[i],type="C-classification")
	      #create an svm with linear kernel 
	      correct=(table(dataset$Severity==fitted(models2[[k]])))["TRUE"] # calculate the no. of correct classifications
	      incorrect=(table(dataset$Severity==fitted(models2[[k]])))["FALSE"] #calculate the no. of incorrect classifications
	      accuracy[k]=(incorrect)/(correct+incorrect)*100 #calculate the percentage error
	      k=k+1
	    }
	    else #for degree greater than 1
	    {
	      models2[[k]]=svm(dataset$Severity~.,data=dataset,kernel="polynomial",cost=cost1[i],type="C-classification",degree=j)
	      #create an with a polynomial kernel and degree as per the current iteration of j
	      correct=(table(dataset$Severity==fitted(models2[[k]])))["TRUE"] # calculate the no. of correct classifications
	      incorrect=(table(dataset$Severity==fitted(models2[[k]])))["FALSE"] #calculate the no. of incorrect classifications
	      accuracy[k]=(incorrect)/(correct+incorrect)*100 #calculate the percentage error
	      k=k+1
	    }
	  }
	}
	
	# Creation of a dataframe that summarizes all the calculated values
	for(i in 1:6) #for each of the six values in cost vector
	{
	  for(j in 1:4) #for degree ranging from 1 through 4
	  {
	    degree[m]=j # create a degree vector 
	    cost[m]=cost1[i] #create a cost vector
	    m=m+1
	  }
	    
	}
	final.df=data.frame(cost,degree,accuracy,error) #create a dataframe
	colnames(final.df)=c("COST","DEGREE","MODEL ERROR","CROss-VALIDATION ERROR") #Give the column names
  print(final.df)# print dataframe
  print(min(accuracy)) #print least error in model errors
  print(min(error)) #printleast error among the cross-validation errors
}			
