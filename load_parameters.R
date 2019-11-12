
rm(list=ls())

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC") # Sets working directory


##### Loading Parameters ##### 

inputs681<-read.table("input_data_681.dat",header=FALSE)  # Reads in parameter file for 681 simulations
inputs1233<-read.table("input_data_1233.dat",header=FALSE)  # Reads in parameter file for 681 simulations
names(inputs681)<-c("AR","Camber","Re") # Sets names to columns
names(inputs1233)<-c("AR","Camber","Re") # Sets names to columns

# Create factor data class columns for each parameter, record levels
inputs681$ARFac<-as.factor(inputs681$AR) 
inputs1233$ARFac<-as.factor(inputs1233$AR) 
levels618.AR<-levels(inputs681$ARFac)
levels1233.AR<-levels(inputs1233$ARFac)

inputs681$CamberFac<-as.factor(inputs681$Camber)
levels.camber<-levels(inputs681$CamberFac)
##### Generating Airfoil dat files #####

ASwing<-read.csv("AS6091.csv",header=TRUE)  
# Calculates original camber based on camber calculation done by Jonathan 
camber.original<-(max(ASwing$y)-min(ASwing$y))/(max(ASwing$x)-min(ASwing$x)) 

# Opens and writes dat files
sink("AS6091_base.dat")
cat("AS6091\ camber=",camber,"\n")
sink()
write.table(ASwing,file="AS6091_base.dat",append=TRUE,col.names=FALSE,row.names=FALSE,sep="  ")

# Calculates new camber and moves points appropriately
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




##### Other Random Calculations, scratch work ####

# Functions
calcAR<-function(AR.new,C){0.5*AR.new*C} # Calculates wing length from aspect ratio
calcSpeeds<-function(Re,C){  # Calculates air speed from Reynolds number
  nu=1.5e-05
  return((Re*nu)/C)
} 

calcAR(as.numeric(as.character(levels.AR[1])),0.1)

calcSpeeds(as.numeric(as.character(levels.Re[1])),0.1)

runs<-parameters[parameters$Camber==levels.Camber[2],]
nrow(parameters[parameters$Camber==levels.Camber[23],])

progress<-0
for(i in 1:21){
progress<-progress+nrow(parameters[parameters$Camber==levels.Camber[i],])
}
progress/681
  
calcAR(10.098076,0.1)



