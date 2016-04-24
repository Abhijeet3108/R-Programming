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

