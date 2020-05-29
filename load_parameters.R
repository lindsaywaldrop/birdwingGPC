
rm(list=ls())

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC") # Sets working directory

# Functions
calcAR<-function(AR.new,C){0.5*AR.new*C} # Calculates wing length from aspect ratio
calcSpeeds<-function(Re,C){  # Calculates air speed from Reynolds number
  nu=1.5e-05
  return((Re*nu)/C)
} 



##### Loading Parameters ##### 

inputs681<-read.table("input_data_681.dat",header=FALSE)  # Reads in parameter file for 681 simulations
inputs1233<-read.table("input_data_1233.dat",header=FALSE)  # Reads in parameter file for 681 simulations
names(inputs681)<-c("AR","Camber","Re") # Sets names to columns
names(inputs1233)<-c("AR","Camber","Re") # Sets names to columns

# Create factor data class columns for each parameter, record levels
inputs681$ARFac<-as.factor(inputs681$AR) 
inputs1233$ARFac<-as.factor(inputs1233$AR) 
inputs681$speeds<-calcSpeeds(inputs681$Re,0.1)
inputs1233$speeds<-calcSpeeds(inputs1233$Re,0.1)
levels681.AR<-levels(inputs681$ARFac)
levels1233.AR<-levels(inputs1233$ARFac)

inputs681$CamberFac<-as.factor(inputs681$Camber)
levels.Camber<-levels(inputs681$CamberFac)
inputs681$ARnumber<-as.numeric(inputs681$ARFac)

##### Generating Airfoil dat files #####

ASwing<-read.csv("AS6091.csv",header=TRUE)  
# Calculates original camber based on camber calculation done by Jonathan 
camber.original<-(max(ASwing$y)-min(ASwing$y))/(max(ASwing$x)-min(ASwing$x)) 

# Opens and writes dat files
sink("AS6091_base.dat")
cat("AS6091\ camber=",camber,"\n")
sink()
write.table(ASwing,file="AS6091_base.dat",append=TRUE,col.names=FALSE,row.names=FALSE,sep="  ")

# Calculates new camber and moves points appropriately for flatwing and panelwing
for (i in 1:length(levels.Camber)){
  camber.new<-as.numeric(as.character(levels.Camber[i]))
  message("camber ",i,"= ",camber.new)
  ASnew.y<-(ASwing$y-min(ASwing$y))*camber.new/(max(ASwing$y)-min(ASwing$y))
  plot(ASwing$x,ASnew.y,col="red")
  points(ASwing$x,ASwing$y,col="blue")
  camber.check<-(max(ASnew.y)-min(ASnew.y))/(max(ASwing$x)-min(ASwing$x))
  message(all.equal(camber.new,camber.check))
  ASnew<-data.frame(x=ASwing$x,y=ASnew.y)
  
  sink(paste("AS6091_cam",i,".dat",sep=""))
  cat("AS6091\ camber=",camber.check,"\n")
  sink()
  #write.table(c("wing"),file="birdwing.dat",append=FALSE,col.names=FALSE,row.names=FALSE,sep="  ")
  write.table(ASnew,file=paste("AS6091_cam",i,".dat",sep=""),append=TRUE,col.names=FALSE,row.names=FALSE,sep="  ")
}


basedir<-"~/Dropbox (Chapman)/wing_gPC/birdwingGPC/taperwingGPC/" # Sets working director
# Calculates new camber and moves points appropriately for taperwing
for (i in 2:length(levels.Camber)){
  setwd(paste(basedir,"Cam",i,"/",sep=""))
  camber.new<-as.numeric(as.character(levels.Camber[i]))
  camber.end<-as.numeric(as.character(levels.Camber[1]))
  message("camber ",i,"= ",camber.new,"; camber end = ",camber.end)
  slope=(camber.new-camber.end)/(0.2-1.0)
  yint=-slope*0.2+camber.new
  AS.new_y1<-(ASwing$y-min(ASwing$y))*camber.new/(max(ASwing$y)-min(ASwing$y))
  #AS.new_y5<-(ASwing$y-min(ASwing$y))*camber.end/(max(ASwing$y)-min(ASwing$y))
  plot(0,0,ylim=range(AS.new_y1),xlim=range(ASwing$x))
  colors=c("red","orange","yellow","green","blue")
  for (j in 1:5){
    if(j==5){camber.now<-camber.end} else{camber.now<-slope*(j*0.2)+yint}
    AS.new_y<-(ASwing$y-min(ASwing$y))*camber.now/(max(ASwing$y)-min(ASwing$y))
    ASnew<-data.frame(x=ASwing$x,y=AS.new_y)
    lines(ASwing$x,AS.new_y,col=colors[j])
    camber.check<-(max(AS.new_y)-min(AS.new_y))/(max(ASwing$x)-min(ASwing$x))
    message("Camber ",i," Pos ",j,": ",all.equal(camber.now,camber.check))
    sink(paste("AS6091_cam",i,"_pos_",j*0.2,".dat",sep=""))
    cat("AS6091\ camber=",camber.now,"\n")
    sink()
    #write.table(c("wing"),file="birdwing.dat",append=FALSE,col.names=FALSE,row.names=FALSE,sep="  ")
    write.table(ASnew,file=paste("AS6091_cam",i,"_pos_",j*0.2,".dat",sep=""),append=TRUE,col.names=FALSE,row.names=FALSE,sep="  ")
  }
}


##### Other Random Calculations, scratch work ####


calcAR(as.numeric(as.character(levels681.AR[1])),0.1)

calcSpeeds(as.numeric(as.character(levels.Re[1])),0.1)

runs<-inputs681[inputs681$Camber==levels.Camber[5],]
nrow(inputs681[inputs681$Camber==levels.Camber[2],])

progress<-0
for(i in 1:8){
progress<-progress+nrow(parameters[parameters$Camber==levels.Camber[i],])
}
progress/681
  
calcAR(10.098076,0.1)

#### Wing area calculation for diamond wings ####

# Aspect ratio = (2*wing length)^2 / wing area

rm(list=ls())

C<-c(0.059,0.140,0.139,0.129,0.099,0.034)
offsets<-c(0,-0.03,-0.04,-0.03,-0.01,0.03)
L<-seq(0,2,by=0.001)

wingarea<-function(L,C,offsets){
  Lm<-L/5
  a1<-rep(NA,length(C)-1)
  a2<-rep(NA,length(C)-1)
  a3<-rep(NA,length(C)-1)
  for(i in 1:length(C)-1){
    a1[i]<-C[i]*Lm
    a2[i]<-0.5*Lm*(C[i+1]-C[i]+(offsets[i+1]-offsets[i]))
    a3[i]<-0.5*Lm*(-offsets[i+1]+offsets[i])
  }
  return(2*(sum(a1)+sum(a2)+sum(a3)))
}

AR<-rep(NA,length(L))
for (k in 1:length(L)){
  AR[k]<-(2*L[k])^2/wingarea(L[k],C,offsets)
}
plot(L,AR)

model<-lm(AR~L)
slope<-as.numeric(model[["coefficients"]][2])

findL<-function(AR){AR/18.06685}
abline(model)

2*L/(mean(C)*L)



