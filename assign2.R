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


