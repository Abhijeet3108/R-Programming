###########################################################

A quadcopter simulator written in R programming that takes input various parameters such as angular velocity, thrust, rotation matrix
etc. and generates an animation of trajectory that the quadcopter will assume based on the same. An OOP based approach was followed 
with the help of S4 classes provided by R.

###########################################################
Quadcopter<- setClass(
  # Set the name for the class
  "Quadcopter",
  # Define the slots
  slots = c(
    id="numeric",
    si ="numeric",
    phi ="numeric",
    theta="numeric",
    omega="numeric",
    xddot="vector",
    xdot="vector",
    x="vector",
    queue="vector",
    YAW="numeric",        
    YAW2="numeric",       
    PITCH="numeric",
    ROLL="numeric",
    MAXIMUMANGLE="numeric",
    THRUSTLIMIT="numeric",
    RESETTHRUSTLIMIT="numeric",
    THRUSTUPOFFSET="numeric",
    THRUSTDOWNOFFSET="numeric",
    PITCHOFFSET="numeric",
    ROLLOFFSET="numeric"
    
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    id=0.0,
    si = 0.0,
    phi = 0.0,
    theta = 0.0,
    omega=2000.0,
    xddot=matrix(c(0,0,0),nrow=3,ncol = 1),
    queue=c(""),
    xdot=0,0,
    x=0.0,
    YAW=1.0,        
    YAW2=5.0,       
    PITCH=-10.0,
    ROLL=10.0,
    MAXIMUMANGLE=6,
    THRUSTLIMIT=1000,
    RESETTHRUSTLIMIT=6000,
    THRUSTUPOFFSET=1000,
    THRUSTDOWNOFFSET=800,
    PITCHOFFSET=1.0,
    ROLLOFFSET=1.0
    
  ),
  
  # Make a function that can test to see if the data is consistent.
  validity=function(object)
  {
    if((object@si < 0) || (object@phi < 0) || (object@theta < 0)) {
      return("A negative number for one of the parameters was given.")
    }
    return(TRUE)
  }
)


################FUNCTION TO SET THE PARAMETERS##############################3
setGeneric(name="SetParameters",
           def=function(theObject,id1,si1,phi1,theta1,omega1)
           {
             standardGeneric("SetParameters")
           }
)

setMethod(f="SetParameters",
          signature="Quadcopter",
          definition=function(theObject,id1,si1,phi1,theta1,omega1)
          {
            theObject@id <- id1
            theObject@si <- si1
            theObject@phi <- phi1
            theObject@theta <- theta1
            theObject@omega <- omega1
            return(theObject)
          }
)
  


#########################FUNCTION TO CALCULATE THE MOVEMENT MATRIX#####################
setGeneric(name="PhysicalBehaviour",
           def=function(theObject,id1,si1,phi1,theta1,omega1)
           {
             standardGeneric("PhysicalBehaviour")
           }
)

setMethod(f="PhysicalBehaviour",
          signature="Quadcopter",
          definition=function(theObject)
          {
            #Defining the constants
            g=9.81
            m=0.468
            k=2.98*10^-6
            
            #Calculating the movement matrix
           si=theObject@si
           phi=theObject@phi
           theta=theObject@theta
           omega=theObject@omega
           T=k*4*omega^2
           
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
           xddot1=-g*mat1+(T/m)*rotation
           theObject@xddot=xddot1
           return(theObject)
           
          }
)

##############Function to return the current position of the Quadcopter####################
setGeneric(name="GetPosition",
           def=function(theObject)
           {
             standardGeneric("GetPosition")
           }
)

setMethod(f="GetPosition",
          signature="Quadcopter",
          definition=function(theObject)
          {
            
            return(theObject@xddot)
          }
)


##################Function to display the helicopter##########################

setGeneric(name="Display",
           def=function(theObject,theObject2)
           {
             standardGeneric("Display")
           }
)

setGeneric(name="Display1",
           def=function(theObject)
           {
             standardGeneric("Display1")
           }
)

setMethod(f="Display1",
          signature="Quadcopter",
         definition=function(theObject)
          {
            library(scatterplot3d)
            x=c(0,0,-20,20)
            y=c(-20,20,0,0)
            z=c(rep(0,4))
            xddot=theObject@xddot
            
           for(i in 1:5)
            {
              
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
              dev.off()
            }
            shell('convert *.png -delay 1x2000 -loop 2 animation.gif')
            file.remove(list.files(pattern=".png"))
            return(theObject)
              
          }
)


setGeneric(name="Rotation",
           def=function(theObject)
           {
             standardGeneric("Rotation")
           }
)

setMethod(f="Rotation",
          signature="Quadcopter",
          definition=function(theObject)
          {
            #Defining the constants
            g=9.81
            m=0.468
            k=2.98*10^-6
            
            #Calculating the movement matrix
      
            mat11=cos(phi*pi/180)*cos(si*pi/180)-cos(theta*pi/180)*sin(phi*pi/180)*sin(si*pi/180)
            mat22=-cos(si*pi/180)*sin(phi*pi/180)-cos(phi*pi/180)*cos(theta*pi/180)*sin(si*pi/180)
            mat33=sin(theta*pi/180)*sin(si*pi/180)
            matr11=cos(theta*pi/180)*cos(si*pi/180)*sin(phi*pi/180)+cos(phi*pi/180)*sin(si*pi/180)
            matr22=cos(theta*pi/180)*cos(si*pi/180)*cos(phi*pi/180)-sin(phi*pi/180)*sin(si*pi/180)
            matr33=-cos(si*pi/180)*sin(theta*pi/180)
            matri12=sin(phi*pi/180)*sin(theta*pi/180)
            matri22=cos(phi*pi/180)*sin(theta*pi/180)
            matri33=cos(theta*pi/180)
            col1=cos(si*pi/180)*sin(theta*pi/180)*cos(phi*pi/180)+sin(si*pi/180)*sin(phi*pi/180)
            col2=sin(si*pi/180)*sin(theta*pi/180)*cos(phi*pi/180)-cos(si*pi/180)*sin(phi*pi/180)
            col3=cos(theta*pi/180)*cos(phi*pi/180)
            rotation=matrix(c(a1,a2,a3),nrow=3,ncol=1)
            Tbb=matrix(c(0,0,sum(omega^2)),nrow=3,ncol=1) 
            mat1=matrix(c(0,0,1),nrow=3,ncol=1)
            xddot1=-g*mat1+(T/m)*rotation
            theObject@xddot=xddot1
            return(theObject)
            
          }
)


setMethod(f="Display",
          signature="Quadcopter",
          definition=function(theObject,theObject2)
          {
            library(rgl)
            #rgl.open()
            id=rgl.cur()
            #xddot=theObject@xddot
            xddot=structure(list(X=c((theObject@xddot[1,]/5),(theObject@xddot[1,]*2/5),(theObject@xddot[1,]*3/5),(theObject@xddot[1,]*4/5),(theObject@xddot[1,])),Y=c((theObject@xddot[2,]/5),(theObject@xddot[2,]*2/5),(theObject@xddot[2,]*3/5),(theObject@xddot[2,]*4/5),(theObject@xddot[2,])),Z=c((theObject@xddot[3,]/5),(theObject@xddot[3,]*2/5),(theObject@xddot[3,]*3/5),(theObject@xddot[3,]*4/5),(theObject@xddot[3,]))),.Names=c("X","Y","Z"),class="data.frame")
            xddot2=structure(list(X=c((theObject2@xddot[1,]/5),(theObject2@xddot[1,]*2/5),(theObject2@xddot[1,]*3/5),(theObject2@xddot[1,]*4/5),(theObject2@xddot[1,])),Y=c((theObject2@xddot[2,]/5),(theObject2@xddot[2,]*2/5),(theObject2@xddot[2,]*3/5),(theObject2@xddot[2,]*4/5),(theObject2@xddot[2,])),Z=c((theObject2@xddot[3,]/5),(theObject2@xddot[3,]*2/5),(theObject2@xddot[3,]*3/5),(theObject2@xddot[3,]*4/5),(theObject2@xddot[3,]))),.Names=c("X","Y","Z"),class="data.frame")
            plot3d(0,0,0,xlim=c(0,100),
                   ylim=c(0,100),
                   zlim=c(0,100))
            print(xddot$Z)
            print(id)
            
            z=c(rep(0,4))
            #x=c(-5,5,5,-5)
            x=c(-15,15,15,-15)
            y=c(0.5,0.5,-0.5,-0.5)
            x2=c(-0.5,0.5,0.5,-0.5)
            #y2=c(5,5,-5,-5)
            y2=c(15,15,-15,-15)
            a1=polygon3d(x,y,z)
            a2=polygon3d(x2,y2,z)
            a3=polygon3d(x,y,z)
            a4=polygon3d(x2,y2,z)
            #polygon3d(x,y,z)
            #polygon3d(x2,y2,z)
            #sphereid <- spheres3d(dat[1,c("X", "Y", "Z")], col="red", radius=1) 
            #print(sphereid)
            spin <- spin3d()
            
            f<-function(time)
            {
              par3d(skipRedraw = TRUE) # stops intermediate redraws 
              on.exit(par3d(skipRedraw=FALSE)) # redraw at the end
              rgl.pop(id=a1)
              rgl.pop(id=a2)
              rgl.pop(id=a3)
              rgl.pop(id=a4)
              pt <- time %% 40 + 1
              pnt <- xddot[pt, c("X", "Y", "Z")]
              pnt2 <- xddot2[pt, c("X", "Y", "Z")]
              a1<<-polygon3d(c(pnt$X-15,pnt$X+15,pnt$X+15,pnt$X-15),c(pnt$Y+0.5,pnt$Y+0.5,pnt$Y-0.5,pnt$Y-0.5),c(pnt$Z,pnt$Z,pnt$Z,pnt$Z))
              a3<<-polygon3d(c(pnt2$X-15,pnt2$X+15,pnt2$X+15,pnt2$X-15),c(pnt2$Y+0.5,pnt2$Y+0.5,pnt2$Y-0.5,pnt2$Y-0.5),c(pnt2$Z,pnt2$Z,pnt2$Z,pnt2$Z))
              a2<<-polygon3d(c(pnt$X-0.5,pnt$X+0.5,pnt$X+0.5,pnt$X-0.5),c(pnt$Y+15,pnt$Y+15,pnt$Y-15,pnt$Y-15),c(pnt$Z,pnt$Z,pnt$Z,pnt$Z))
              a4<<-polygon3d(c(pnt2$X-0.5,pnt2$X+0.5,pnt2$X+0.5,pnt2$X-0.5),c(pnt2$Y+15,pnt2$Y+15,pnt2$Y-15,pnt2$Y-15),c(pnt2$Z,pnt2$Z,pnt2$Z,pnt2$Z))
              #polygon3d(c(pnt$X-5,pnt$X+5,pnt$X+5,pnt$X-5),c(pnt$Y+0.15,pnt$Y+0.15,pnt$Y-0.15,pnt$Y-0.15),c(pnt$Z,pnt$Z,pnt$Z,pnt$Z))
              #polygon3d(c(pnt$X-0.15,pnt$X+0.15,pnt$X+0.15,pnt$X-0.155),c(pnt$Y+5,pnt$Y+5,pnt$Y-5,pnt$Y-5),c(pnt$Z,pnt$Z,pnt$Z,pnt$Z))
              spin(time)
            }
            play3d(f,duration=Inf)
          }
)


######################Send Function###############################
setGeneric(name="Send",
           def=function(theObject1,theObject2,message)
           {
             standardGeneric("Send")
           }
)

setMethod(f="Send",
          signature="Quadcopter",
          definition=function(theObject1,theObject2,message)
          {
            id1 <- theObject1@id
            id2 <- theObject2@id
            #queue1=theObject2@queue
            #theObject2@queue=union(theObject2@queue,message)
            slot(theObject2,"queue")=union(slot(theObject2,"queue"),message)
            return(theObject2)
          }
)





setGeneric(name="QuadActions",
           def=function(theObject)
           {
             standardGeneric("QuadActions")
           }
)

setMethod(f="QuadActions",
          signature="Quadcopter",
          definition=function(theObject)
          {
            actionCompleted=FALSE
            cstate=NULL
            desiredPosition=c()
            desiredAngle=0.0
            desiredDistance
            maxTime=0
            minTime=0
            
            previousPitch = 0.0;
            pitchSteps = 0;
            previousRoll = 0.0;
            rollSteps = 0;
            previousThrust = 0;
            thrustSteps = 0;
            
            
            if(!actionCompleted)
            {
            switch(cstate,
                   NULL_ACTION={
                     stayIdle()
                   },
                   UP_ACTION={
                     actionCompleted=moveUp()
                     
                   },
                   DOWN_ACTION={
                     actionCompleted=moveDown()
                   },
                   HOVER_ACTION={
                     actionCompleted=keepHovering()
                   },
                   ROTATE_RIGHT_ACTION={
                     actionCompleted=rotateRight()
                     
                   },
                   ROTATE_LEFT_ACTION={
                     actionCompleted=rotateLeft()
                   },
                   RIGHT_ACTION={
                     actionCompleted=moveRight()
                   },
                   LEFT_ACTION={
                     actionCompleted=moveLeft()
                   },
                   STRAIGHT_ACTION={
                     actionCompleted=moveStraight()
                   },
                   stop("Wrong action called!")
                   )
            }
            
            setPitch<-function(x)
            {
              if (x == 0)
              {
                if ((previousPitch > 0.01 || previousPitch < -0.01) && pitchSteps > 0)
                {
                  previousPitch =  -previousPitch/2.0
                  pitchSteps = 5
                }
                pitchSteps=pitchSteps-1
              }
              else {
                previousPitch =  (5 * x)
                pitchSteps = 5
              }
              targetVal.pitch = previousPitch
            }
            
            
            setRoll<-function(y)
            {
              if (y == 0)
              {
                if ((previousRoll > 0.01 || previousRoll < -0.01) && rollSteps > 0)
                {
                  previousRoll =  -previousRoll/2.0
                  rollSteps = 5
                }
                rollSteps=rollSteps-1
              }
              else
              {
                previousRoll =  (5 * y)
                rollSteps = 5
              }
              targetVal.roll = previousRoll
              
            }
            
            keepHovering<-function()
            {
              setPitch()
              setRoll()
              setThrust()
              targetVal.yaw = 0  
              
              if (minTime <= 0)
                return(TRUE)
              else
              return(FALSE)
              
            }
            
            stayIdle=function()
            {
              targetVal.thrust = 0
              targetVal.yaw = 0
              targetVal.roll = 0
              targetVal.pitch = 0
              
            }
            
            moveStraigth=function()
            {
              if (maxTime <= 0)
                return(TRUE)
              else
                return(FALSE)
              
            }
                 
            rotateLeft=function()
            {
              keepHovering()
              targetVal.yaw = YAW;
              if (desiredAngle <= 0)
                return(TRUE)
              else
                desiredAngle =desiredAngle-YAW/2.0
                return(FALSE)
                
            }
            
            rotateRight=function(){
              keepHovering()
              targetVal.yaw = -YAW
              if (desiredAngle <= 0)
                return(TRUE)
              else
                desiredAngle =desiredAngle-YAW/2.0
                return(FALSE)
                
            }
            
            moveRight=function(){
              keepHovering()
              if (desiredAngle >= 0) 
              {
                targetVal.yaw = YAW/2.0
                targetVal.pitch =targetVal.pitch+PITCH
                targetVal.thrust =targetVal.thrust+ 10
                desiredAngle = 0
                return(FALSE)
              }
              return(TRUE)
              
            }
            
            moveLeft=function(){
              keepHovering()
              if (desiredAngle >= 0) 
              {
                targetVal.yaw = targetVal.yaw-YAW2
                targetVal.pitch = targetVal.pitch+ PITCH;
                targetVal.thrust =targetVal.thrust+10;
                return(FALSE)
              }
              return(TRUE)
              
            }
          }
)
