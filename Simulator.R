simulator=function()
{
  library(scatterplot3d)
  #j=1
  #x=c(50,50,40,60)
  #y=c(40,60,50,50)
  si=as.integer(readline(prompt="Enter si: "))
  phi=as.integer(readline(prompt="Enter phi: "))
  theta=as.integer(readline(prompt="Enter theta: "))
  omega=as.integer(readline(prompt="Enter omega(velocity:) "))
  #omega=3000
  g=9.81
  m=0.468
  k=2.98*10^-6
  T=k*4*omega^2
  #T=3000
  x=c(0,0,-20,20)
  y=c(-20,20,0,0)
  z=c(rep(0,4))
  a11=cos(phi*pi/180)*cos(si*pi/180)-cos(theta*pi/180)*sin(phi*pi/180)*sin(si*pi/180)
  a22=-cos(si*pi/180)*sin(phi*pi/180)-cos(phi*pi/180)*cos(theta*pi/180)*sin(si*pi/180)
  a33=sin(theta*pi/180)*sin(si*pi/180)
  b11=cos(theta*pi/180)*cos(si*pi/180)*sin(phi*pi/180)+cos(phi*pi/180)*sin(si*pi/180)
  b22=cos(theta*pi/180)*cos(si*pi/180)*cos(phi*pi/180)-sin(phi*pi/180)*sin(si*pi/180)
  b33=-cos(si*pi/180)*sin(theta*pi/180)
  c12=sin(phi*pi/180)*sin(theta*pi/180)
  c22=cos(phi*pi/180)*sin(theta*pi/180)
  c33=cos(theta*pi/180)
  a1=cos(si*pi/180)*sin(theta*pi/180)*cos(phi*pi/180)+sin(si*pi/180)*sin(phi*pi/180)
  a2=sin(si*pi/180)*sin(theta*pi/180)*cos(phi*pi/180)-cos(si*pi/180)*sin(phi*pi/180)
  a3=cos(theta*pi/180)*cos(phi*pi/180)
  rotation=matrix(c(a1,a2,a3),nrow=3,ncol=1)
  Tbb=matrix(c(0,0,sum(omega^2)),nrow=3,ncol=1) 
  mat1=matrix(c(0,0,1),nrow=3,ncol=1)
  xddot=-g*mat1+(T/m)*rotation
  print(xddot)
  for(i in 1:5)
  {
    #p2 <- pxyz.convert(x[2],y[2],z[2])
    #j=j+10
    #Fd=matrix(c(-kd*xdot,-kd*ydot,-kd*zdot),nrow=3,ncol=1
    png(paste(i,'plot.png',sep=''))
    s=scatterplot3d(x,y,z,
                    main="Quadcopter Simulator",
                    xlab = "X-axis",
                    ylab = "Y-axis",
                    zlab = "Z-axis",
                    xlim=c(-100,200),
                    ylim=c(-100,200),
                    zlim=c(-100,200),
                    box=T)
    p1 <- s$xyz.convert(x[1],y[1],z[1])
    p2 <- s$xyz.convert(x[2],y[2],z[2])
    p3 <- s$xyz.convert(x[3],y[3],z[3])
    p4 <- s$xyz.convert(x[4],y[4],z[4])
    segments(p1$x,p1$y,p2$x,p2$y,lwd=3,col=3)
    segments(p3$x,p3$y,p4$x,p4$y,lwd=3,col=2)
    x[1]=xddot[1]*(i/5)
    x[2]=(xddot[1])*(i/5)
    x[3]=((xddot[1]))*(i/5)-20
    x[4]=((xddot[1]))*(i/5)+20
    y[1]=((xddot[2]))*(i/5)-20
    y[2]=((xddot[2]))*(i/5)+20
    y[3]=(xddot[2])*(i/5)
    y[4]=(xddot[2])*(i/5)
    z[1]=(xddot[3])*(i/5)
    z[2]=(xddot[3])*(i/5)
    z[3]=(xddot[3])*(i/5)
    z[4]=(xddot[3])*(i/5)
    #y=x+xddot[2]/5
    #z=x+xddot[3]/5
    #x=x+10
    #y=y+10
    #z=z+10
    dev.off()
  }
  shell('convert *.png -delay 1x2000 -loop 2 animation.gif')
  #shell('convert $(for a in *; do printf -- "-delay 10 %s " $a; done; ) result.gif"')
  #file.remove(list.files(pattern=".png"))
}

#system('"C:\\Users\\abhi\\Desktop\\simulator" convert -delay 80 *.png example_1.gif',intern = TRUE)
#system("cd C:/Users/abhi/Desktop/simulator")

#system('C:\\Program Files\\ImageMagick-7.0.3-Q16\\convert.exe" -delay 80 *.png example_1.gif',intern = TRUE) 
#system('"C:\\Program Files\\ImageMagick-7.0.3-Q16\\convert.exe" *.png -delay 1000 -loop 0 animation.gif"',intern = TRUE)

#system("cmd.exe convert *.png -delay 1000 -loop 0 animation.gif")