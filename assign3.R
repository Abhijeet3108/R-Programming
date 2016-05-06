###############################################################################################
Problem Statement:

QUESTION 1)

a. Use R’s plot function to plot the positively classified training vectors
(−1, 2),(1, 4),(2, 3),(3, 3),
and negatively classified training vectors
(−2, −3),(1, 0),(4, −1).
Use two different kinds of labels for each respective set of vectors, so that the two sets of
vectors are visually distinguishable.

b. List the support vectors.

c. “Pencil and Paper” Exercise. Provide equatiions (in the form of y = mx + b) for the
two supporting lines, and the line that serves as the maximum-margin classifier. Show the
algebraic steps that you performed to obtain the equations of each line. Note: although
this is a “Pencil and Paper” exercise, you should show your algebraic steps with the help
of a word processor.

d. Add three lines to your plot from Part a): the two supporting lines, and the maximummargin
classifier.

QUESTION 2)

Install the kernlab package into your R system. The package is located at
https://cran.r-project.org/web/packages/kernlab/index.html
For example, if using windows and the “vanilla” R interpreter, download the kernlab 0.9-23.zip
windows binaries zip file. Then in R’s “packages” menu choose option “install packages from
local zip files”. After this, the package must be loaded using the “load package” option from
the same menu. Finally, remember to call library(kernlab) using the command prompt. Use
the ipop function to solve the dual Lagrangian of the Maximum-Margin-Classifies optimization
problem with respect to the training data from Exercise 1. Make sure to provide in your
document the required c, H, A, b, l, u, and r matrices/vectors. Note: ipop solves a minimization
problem, therefore, you must optimize over the negative of the dual Lagrangian. Clearly present
your solution by providing the Lagrange multiplier of each training vector.

QUESTION 3)
Use your solution from Exercise 2 to determine w∗ and b∗ for the maximum-margin classifier.
Convert the solution to the form y = mx+b.

QUESTION 4)
Use R’s plot function to plot the positively classified training vectors
(−2, 2),(0, 1.5),(−1, −2),(1.5, 0),
and negatively classified training vectors
(−0.75, 0),(0.05, −0.5),(0.6, 0.1),(0, 0.75).
Verify that these vectors are not linearly separable.

QUESTION 5)
Use the ipop function to solve the dual Lagrangian non-linear MMC optimization problem
with respect to the training data from Exercise 4, and using a kernel function k(x, y) = (x · y)2.
Make sure to provide in your document the required c, H, A, b, l, u, and r matrices/vectors.
Clearly present your solution by providing the Lagrange multiplier of each training vector.

QUESTION 6)
Use your solution from Exercise 5 to determine b∗
for the classifier
ˆf(x) = sgn(summation(α∗iyi(xi, x)2 − b∗).

Question 7)
Use your non-linear classifier from Exercise 6 to classify the points (3, 1), (0.25, 0.5), and(−2, −3).

########################################################################################################################
Question 1a)

input<-function(a1)
{
attach(a1)
ncol=length(a1[,1])
pch_list=rep(0,ncol)
pch_list[sym==1]<-3
pch_list[sym==2]<-4
plot(x1,y1,pch=c(pch_list))
}
#####################################################################################
Question 1d)

input<-function(a1)
{
attach(a1)
ncol=length(a1[,1])
pch_list=rep(0,ncol)
pch_list[sym==1]<-3
pch_list[sym==2]<-4
plot(x1,y1,pch=c(pch_list))
abline(a=1,b=0.25,col="red") #optimal line
abline(a=2.25,b=0.25,col="green") #positive supporting line
abline(a=-0.25,b=0.25,col="blue") #negative supporting line
}
#######################################################################################

QUESTION 2)

q3<-function(a1)
{
library(kernlab) #importing the kernlab library
attach(a1)
nrows=length(a1[,1]) #number of rows
l=rep(0,7) #Lower bound vector or one column matrix
u=rep(9999,7) #Upper bound vector or one column matrix
A=c(1,1,1,1,-1,-1,-1)
b=0
r=0
c = rep(-1,7)
H=matrix(nrow=7,ncol=7)
y=rep(0,nrows)
y[sign==1]=1
y[sign==-1]=-1
for(i in 1:dim(H)[1])
{
for(j in 1:dim(H)[1])
{
xi=unlist(a1[i,c(1,2)])
xj=unlist(a1[j,c(1,2)])
H[i,j] = y[i] * y[j] * (xi%*%xj) #Calculate H[i][j] of matrix
}
}
solution <- ipop(c, H, A, b, l, u, r) #use the ipop function with the mentioned parameters
return (solution)
}

#####################################################################################

Question 3)

w=c(0,0) #initiliaze to 0,0
nrows=length(a1[,1]) #number of rows
y=rep(0,nrows)
y[sign==1]=1
y[sign==-1]=-1
for(i in 1:nrows)
{
xi=unlist(a1[i,c(1,2)])
w=w+primal(solution)[i]*xi*y[i] #keep adding the w values at each instance
}
print(w)

######################################################################################

QUESTION 4)

input=function(a2)
{
attach(a2)
nrows=length(a1[,1])
pch_list=rep(0,nrows)
pch_list[sign==1]<-3
pch_list[sign==-1]<-4
plot(a2$x,a2$y,pch=c(pch_list))
}

###################################################################################

QUESTION 5)

input<-function(a1)
{
	library(kernlab)
	#attach(a1)
	nrows=length(a1[,1])
	#ncols=length(a1[1,])
	l=rep(0,8)
	u=c(9999,9999,9999,9999,9999,9999,9999,9999)
	A=c(1,1,1,1,-1,-1,-1,-1)
	b=0
	r=0
	c = rep(-1,8)
	H=matrix(nrow=8,ncol=8)
	y=rep(0,8)
	y[sign==1]=1
	y[sign==-1]=-1
	for(i in 1:dim(H)[1])
	{	
		for(j in 1:dim(H)[1])
		{
			xi=unlist(a1[i,c(1,2)]) 
			xj=unlist(a1[j,c(1,2)]) 
			H[i,j] = y[i] * y[j] * (xi%*%xj)* (xi%*%xj)      
		}

	}
print(H)
solution <- ipop(c, H, A, b, l, u, r)
sum=0
b1=0
x4=c(1.5,0)
for(i in 1:dim(H)[1])
{
	xi <- unlist(a1[i,c(1,2)])  
	sum = sum + primal(solution)[i]*y[i]*(xi%*%x4)*(xi%*%x4)
}
b1=sum-1
print(b1)

################################################################################################

QUESTION 6)

solution <- ipop(c, H, A, b, l, u, r)
sum=0
b1=0
x4=c(1.5,0)
for(i in 1:dim(H)[1])
{
xi <- unlist(a1[i,c(1,2)])
sum = sum + primal(solution)[i]*y[i]*(xi%*%x4)*(xi%*%x4)
}
b1=sum-1
print(b1)


###################################################################################################

QUESTION 7)

sign1=0
sign2=0
sign3=0
sum1=0
sum2=0
sum3=0
p1=c(3,1)
p2=c(0.25,0.5)
p3=c(-2,-3)
for(i in 1:dim(H)[1])
{
	xi <- unlist(a1[i,c(1,2)])  
	sum1 = sum1 + primal(solution)[i]*y[i]*(xi%*%p1)*(xi%*%p1)
	sum2 = sum2 + primal(solution)[i]*y[i]*(xi%*%p2)*(xi%*%p2)
	sum3 = sum3 + primal(solution)[i]*y[i]*(xi%*%p3)*(xi%*%p3)
}
sign1=sign(sum1-1.666667)
sign2=sign(sum2-1.666667)
sign3=sign(sum3-1.666667)
print(sign1)
print(sign2)
print(sign3)

}


###########################OUTPUT###############################

Matrix H:
[,1] [,2] [,3] [,4] [,5] [,6] [,7]
[1,]    5    7    4    3    4    1    6
[2,]    7   17   14   15   14   -1    0
[3,]    4   14   13   15   13   -2   -5
[4,]    3   15   15   18   15   -3   -9
[5,]    4   14   13   15   13   -2   -5
[6,]    1   -1   -2   -3   -2    1    4
[7,]    6    0   -5   -9   -5    4   17
> primal(t) # list of α-variables
[1] 2.200000e-01 1.256540e-09 1.237462e-08 1.200000e-01 1.088293e-09
[6] 3.400000e-01 1.572324e-09
> how(t)
[1] "converged"
> dual(t) 
[1] 0.8
Therefore, list of Lagrange multipliers approximately are:
∝_1=0.22,∝_2=0,∝_3=0,∝_4=0.12,∝_5=0,∝_6=0.34,∝_7=0.

Value of w:
   x    y 
-0.2  0.8



Value of Matrix H:
      [,1]     [,2]    [,3]    [,4]       [,5]       [,6]        [,7]       [,8]
[1,] 64.00  9.000000  4.0000  9.000000 -1.21000000 -2.25000000 -2.2500000 -1.000000
[2,]  9.00  5.062500  9.0000  0.000000 -0.56250000  0.00000000 -1.2656250 -0.022500
[3,]  4.00  9.000000 25.0000  2.250000 -0.90250000 -0.56250000 -2.2500000 -0.640000
[4,]  9.00  0.000000  2.2500  5.062500 -0.00562500 -1.26562500  0.0000000 -0.810000
[5,] -1.21 -0.562500 -0.9025 -0.005625  0.06375625  0.00140625  0.1406250  0.000400
[6,] -2.25  0.000000 -0.5625 -1.265625  0.00140625  0.31640625  0.0000000  0.202500
[7,] -2.25 -1.265625 -2.2500  0.000000  0.14062500  0.00000000  0.3164062  0.005625
[8,] -1.00 -0.022500 -0.6400 -0.810000  0.00040000  0.20250000  0.0056250  0.136900

> primal(t1)
[1] 7.771423e-11 6.925947e-01 7.689207e-10 7.120692e-01 6.793649e-09 7.412809e-01 6.633830e-01 1.100398e-08
> how(t1)
[1] "converged"
> dual(t1)
[1] 1.666667

Therefore, the calculated Lagrange multipliers are as follows:
∝_1=0,∝_2=0.692,∝_3=0,∝_4=0.712, ∝_5=0.741,∝_6=0,∝_7=0.663,∝_8=0.
