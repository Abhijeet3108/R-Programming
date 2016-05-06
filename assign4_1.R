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



####################OUTPUT########################

COST      DEGREE  MODEL ERROR 		CROSS-VALIDATION ERROR
1  1e-01      1   17.481790               18.21020
2  1e-01      2   46.305931               46.30593
3  1e-01      3   46.305931               46.30593
4  1e-01      4   46.305931               46.30593
5  1e+00      1   16.233091               18.00208
6  1e+00      2   46.305931               46.30593
7  1e+00      3   46.305931               46.30593
8  1e+00      4   46.305931               46.30593
9  1e+01      1   15.608741               18.31426
10 1e+01      2   17.481790               18.52237
11 1e+01      3   46.305931               46.30593
12 1e+01      4   46.305931               46.30593
13 1e+02      1   15.504683               18.41831
14 1e+02      2   14.151925               18.93861
15 1e+02      3   18.002081               18.31426
16 1e+02      4   46.305931               46.30593
17 1e+03      1   15.504683               19.04266
18 1e+03      2   10.926119               17.89802
19 1e+03      3   15.088450               18.73049
20 1e+03      4   46.305931               46.30593
21 1e+04      1   15.504683               18.83455
22 1e+04      2    5.931322               21.95630
23 1e+04      3   10.405827               17.79396
24 1e+04      4   16.129032               19.56296
[1] 5.931322
[1] 17.79396
