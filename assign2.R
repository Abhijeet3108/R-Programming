#######################################################################################################################
PROBLEM STATEMENT :

QUESTION 1)
a. Use R’s plot function to plot each (x1, x2) pair, labeling each point in the +1 class with
a “+”, and each point in the −1 class with an open circle. Does the data appear linearly
separable?
b. For the data, compute c+, c−, cmid, and c+ − c− (see Section 4.2).
c. Use the vectors from the previous part to classify each observation in exercise-1.csv.
Report on the classification accuracy (i.e. percentage correct).
d. Use the vectors from part b to predict the class for the observation (x1, x2) = (2, 2).

QUESTION 2)
a. For the data, compute c+, c−, cmid, and c+ − c− (see Section 4.2). Assume that males
represent class +1, while females represent class −1.
b. Use the vectors from the previous part to classify each observation in exercise-2.csv.
Report on the classification accuracy (i.e. percentage correct).
c. Use the vectors from part a to classify Pat, who is 62 inches tall, weighs 125 pounds, and
is 45 years old.

QUESTION 3)
This exercise pertains to the mammographic mass data set (with missing-data observations
removed).
a. For the data, compute c+, c−, cmid, and c+ −c− (see Section 4.2). Assume that severity-1
observations represent class +1, while severity-0 observations epresent class −1.
b. Use the vectors from the previous part to classify each observation in exercise-2.csv.
Report on the classification accuracy (i.e. percentage correct).
c. Use the vectors from part a to classify a mass that has attribute values (5, 60, 1, 5, 3)
##################################################################################################################

classification<-function(x)
{
	c_plus=c()
	c_minus=c()
	d=c()
	c_mid=c()
	check=c()
	sub1=c()
	real=c()
	result=c()
	count=0
	nrows=length(x[1,]) #nrows contains the no. of rows
	ncol=length(x[,1])	#ncol contains the no. of columns
	
	z=nrows-1
	sub1=subset(x,select=c(1:z))

	if(nrows==3)	#For question 1 of the assignment
	{
		pch_list=rep(0,ncol)	#create an empty vector of 0's 
		pch_list[x$y==-1]<-1		#over-write 0's with 1's wherever y=-1
		pch_list[x$y==1]<-3		#over-write next 0's with 3's wherever y=1
		plot(x$x1,x$x2,pch=c(pch.list)) #plot function plots (x1,x2), with pch=1=o sign and pch=3=+ sign

		c_plus=c(colMeans(subset(x,select=c(1:z),y==1)))
		#first create a subset from columns 1:n-1 and who has y=1. Then, compute mean using colmeans() that gives mean of all the subsetted columns. This is our c_plus.
		c_minus=c(colMeans(subset(x,select=c(1:z),y==-1)))
		#first create a subset from columns 1:n-1 and who has y=-1. Then, compute mean using colmeans() that gives mean of all the subsetted columns. This is our c_minus.
		c_mid=((c(c_plus)+c(c_minus))*0.5) #compute c_mid 
		d=c(c_plus)-c(c_minus)	#compute d
		
		for(i in 1:ncol)	#loop from 1 until the end of datasets(i.e. no column)
		{
			result[i]=sign((c(sub1$x1[i],sub1$x2[i])-c_mid)%*%d) #compute the sign value for each iteration and store in result[i]
			if(x$y[i]==result[i])	#check if predicted sign is equal to actual sign
			count=count+1		#if true increase counter
			
		}

	}

	if(nrows==4)	#for question 2 of the assignment
	{
		c_plus=c(colMeans(subset(x,select=c(1:z),Gender=="M")))
		#first create a subset from columns 1:n-1 and who has Gender="M". Then, compute mean using colmeans() that gives mean of all the subsetted columns. This is our c_plus
		c_minus=c(colMeans(subset(x,select=c(1:z),Gender=="F")))
		#first create a subset from columns 1:n-1 and who has Gender="F". Then, compute mean using colmeans() that gives mean of all the subsetted columns. This is our c_minus.
		c_mid=((c(c_plus)+c(c_minus))*0.5)	#compute c_mid
		d=c(c_plus)-c(c_minus)	#compute d

		for(i in 1:ncol) #loop from 1 until end of dataset.
		{
			result[i]=sign((c(sub1$Height[i],sub1$Weight[i],sub1$Age[i])-c_mid)%*%d) #compute the sign value for each iteration and store it in result[i].
			if(x$Gender[i]=="M") #if for the current iteration Gender=M,real[i]=1. 
			real[i]=1
			else
			real[i]=-1	#else for Gender=F,real[i]=-1
			if(result[i]==real[i])  #if predicted classification matches actual classification increase count by 1
			count=count+1
		}
	}

	if(nrows==6) #for question 3 of the assignment
	{	
		c_plus=c(colMeans(subset(x,select=c(1:z),Severity==1))) #first create a subset from columns 1:n-1 and who has Severity=1. Then, compute mean using colmeans() that gives mean of all the subsetted columns. This is our c_plus.
		c_minus=c(colMeans(subset(x,select=c(1:z),Severity==0)))#first create a subset from columns 1:n-1 and who has Severity=0. Then, compute mean using colmeans() that gives mean of all the subsetted columns. This is our c_minus.
		c_mid=((c(c_plus)+c(c_minus))*0.5)	#compute c_mid
		d=c(c_plus)-c(c_minus)	#compute d	

		for(i in 1:ncol) #loop from 1 to end of dataset.
		{
			result[i]=sign((c(sub1$Birads[i],sub1$Age[i],sub1$Shape[i],sub1$Margin[i],sub1$Density[i])-c_mid)%*%d)	#compute sign value for each iteration and store it in result[i].
			if(x$Severity[i]==1) 
			real[i]=1
			else
			real[i]=-1
			if(result[i]==real[i])
			count=count+1
		}
	}
	print("c_plus:")
	print(c_plus)
	print("c_minus:")
	print(c_minus)
	print("c_mid:")
	print(c_mid)
	print("d:")
	print(d)
	accuracy=(count/ncol)*100 #compute the accuracy.
	cat("Predicted Classification:",result)
	cat("\n")
	cat("Accuracy:",accuracy)
	cat("\n")	

	input(nrows,c_mid,d) #For the last part in each question, this function takes user input and displays the predicted classification
} #end of function

input<-function(nrows,c_mid,d)	
{
	result=0
	if(nrows==3)#for any user input for first question it predicts the classification
	{
	n1<- readline(prompt="x1: ")
	n1=as.integer(n1)
	n2<- readline(prompt="x2: ")
	n2=as.integer(n2)
	result=sign((c(n1,n2)-c_mid)%*%d)
	cat("Classification:",result)
	}
		
	if(nrows==4)#for any user input on second question it predicts the classification.

	{
	n1<- readline(prompt="Height: ")
	n1=as.integer(n1)
	n2<- readline(prompt="Weight: ")
	n2=as.integer(n2)
	n3<- readline(prompt="Weight: ")
	n3=as.integer(n3)
	result=sign((c(n1,n2,n3)-c_mid)%*%d)
	if(result==1)
	cat("Classification:",result,".i.e Male")
	else
	cat("Classification:",result,".i.e Female")
	}

	if(nrows==6)	#for any user input of third question it predicts the classification.

	{
	n1<- readline(prompt="Birads: ")
	n1=as.integer(n1)
	n2<- readline(prompt="Age: ")
	n2=as.integer(n2)
	n3<- readline(prompt="Shape: ")
	n3=as.integer(n3)
	n4<- readline(prompt="Margin: ")
	n4=as.integer(n4)
	n5<- readline(prompt="Density: ")
	n5=as.integer(n5)
	result=sign((c(n1,n2,n3,n4,n5)-c_mid)%*%d)
	if(result==1)
	cat("Classification:",result,"i.e. Severity=1")
	else
	cat("Classification:",result,".i.e Severity=0")
	}
	cat("\n")

}


##################OUTPUT############################################

[1] "c_plus:"
 x1  x2 
3.5 1.5 
[1] "c_minus:"
      x1       x2 
1.666667 3.333333 
[1] "c_mid:"
      x1       x2 
2.583333 2.416667 
[1] "d:"
       x1        x2 
 1.833333 -1.833333
Predicted Classification: -1 -1 -1 1 1
Accuracy: 100
x1: 2
x2: 2
Classification: -1

Question2)
> source("try.R")
> classification(data2)
[1] "c_plus:"
Height Weight    Age 
 71.78 179.52  35.46 
[1] "c_minus:"
Height Weight    Age 
 56.36 110.90  29.26 
[1] "c_mid:"
Height Weight    Age 
 64.07 145.21  32.36 
[1] "d:"
Height Weight    Age 
 15.42  68.62   6.20
Predicted Classification: -1 -1 1 -1 1 -1 1 1 1 1
Accuracy: 90
Height: 62
Weight: 125
Weight: 45
Classification: -1 .i.e Female


Question 3)
> source("try.R")
> classification(data3)
[1] "c_plus:"
   Birads       Age     Shape    Margin   Density 
 4.828784 62.652605  3.503722  3.739454  2.940447 
[1] "c_minus:"
   Birads       Age     Shape    Margin   Density 
 3.983607 49.297424  2.100703  1.939110  2.892272 
[1] "c_mid:"
   Birads       Age     Shape    Margin   Density 
 4.406195 55.975015  2.802212  2.839282  2.916359 
[1] "d:"
     Birads         Age       Shape      Margin     Density 
 0.84517756 13.35518157  1.40301951  1.80034402  0.04817499
Predicted Classification: 1 1 -1 1 1 -1 -1 1 -1 -1 1 -1 1 -1 1 1 -1 -1 -1 -1 1 -1 1 1 1 -1 1 -1 1 1 1 1 1 -1 -1 1 1 1 -1 -1 1 1 1 -1 1 -1 1 1 1 1 1 -1 1 1 1 1 1 1 -1 1 -1 1 -1 1 1 -1 1 -1 1 -1 -1 1 -1 1 -1 1 1 1 1 -1 1 1 -1 1 1 -1 -1 1 1 1 -1 1 1 1 -1 -1 1 1 -1 -1 -1 1 1 1 -1 1 -1 -1 -1 -1 1 -1 1 -1 -1 1 -1 -1 1 -1 1 1 -1 1 1 -1 -1 1 -1 1 1 -1 -1 -1 -1 -1 1 1 -1 -1 -1 1 -1 1 -1 1 1 -1 -1 -1 1 1 -1 -1 1 -1 -1 1 1 1 1 1 -1 1 1 -1 1 1 -1 -1 1 -1 -1 1 -1 -1 -1 1 -1 1 1 1 1 1 1 1 -1 -1 1 -1 1 -1 1 1 1 -1 1 1 1 1 1 1 -1 -1 -1 1 1 1 -1 -1 -1 1 -1 -1 1 1 1 -1 -1 -1 -1 -1 -1 1 -1 -1 -1 -1 -1 -1 -1 1 1 1 -1 1 -1 1 1 1 -1 -1 1 -1 -1 -1 1 1 1 -1 -1 -1 1 1 1 1 -1 -1 1 -1 -1 1 1 1 1 -1 -1 -1 -1 -1 1 -1 -1 -1 -1 1 1 -1 -1 -1 -1 -1 -1 1 1 1 -1 1 -1 1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 -1 1 1 -1 -1 1 1 1 -1 1 1 -1 1 1 1 1 1 1 1 -1 1 -1 -1 1 1 1 1 -1 -1 -1 1 1 1 1 -1 -1 -1 -1 -1 1 1 1 1 -1 -1 1 1 -1 1 1 1 -1 -1 1 1 -1 -1 -1 1 1 1 1 -1 -1 -1 1 1 -1 1 -1 -1 1 1 -1 -1 1 -1 -1 -1 -1 -1 -1 1 -1 -1 -1 -1 -1 -1 -1 -1 1 1 -1 -1 1 -1 -1 -1 -1 -1 -1 -1 1 1 -1 1 1 1 -1 -1 -1 1 -1 1 -1 1 1 1 1 1 1 -1 -1 1 -1 1 -1 -1 1 -1 -1 -1 1 -1 1 1 1 1 1 -1 -1 -1 -1 1 1 1 1 -1 -1 1 1 -1 1 -1 -1 1 -1 -1 -1 -1 -1 -1 1 -1 1 -1 -1 1 -1 -1 -1 1 1 1 1 -1 -1 1 1 1 1 1 1 -1 -1 1 -1 -1 1 1 -1 -1 1 -1 1 1 1 1 1 -1 -1 -1 -1 1 1 1 1 1 1 -1 -1 1 1 -1 1 -1 -1 -1 -1 1 -1 1 -1 1 -1 -1 1 1 1 1 1 -1 1 -1 -1 1 -1 1 -1 -1 1 1 1 -1 -1 -1 -1 1 1 1 1 -1 1 -1 -1 -1 -1 -1 -1 1 -1 1 1 -1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 1 1 1 -1 1 -1 1 -1 1 1 -1 1 -1 1 1 1 -1 -1 1 1 1 1 1 -1 1 -1 1 1 -1 -1 -1 1 1 -1 -1 1 -1 1 -1 -1 1 1 1 1 1 -1 1 -1 1 1 1 1 -1 -1 1 1 1 -1 1 1 -1 1 1 -1 -1 1 1 1 -1 -1 -1 1 -1 -1 1 1 1 1 -1 1 1 -1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 -1 -1 1 -1 1 -1 1 1 1 1 1 1 1 1 1 -1 -1 1 1 -1 1 1 1 1 -1 1 1 -1 1 -1 1 1 -1 1 -1 -1 -1 1 1 -1 -1 -1 1 -1 1 1 -1 -1 -1 -1 1 -1 1 -1 -1 1 1 1 -1 1 -1 -1 1 -1 -1 -1 1 1 -1 -1 1 1 -1 -1 -1 -1 -1 -1 -1 -1 1 -1 1 1 -1 1 -1 1 -1 -1 1 1 1 -1 1 -1 -1 -1 1 1 -1 1 1 1 -1 1 -1 -1 1 1 1 -1 -1 -1 -1 -1 1 -1 1 1 -1 -1 1 -1 1 1 -1 1 -1 1 1 1 1 -1 -1 1 1 1 1 1 1 -1 -1 -1 1 1 1 1 -1 -1 -1 1 1 1 1
Accuracy: 68.91566
Birads: 5
Age: 60
Shape: 1
Margin: 5
Density: 3
Classification: 1 i.e. Severity=1

