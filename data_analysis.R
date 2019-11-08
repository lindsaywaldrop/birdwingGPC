rm(list=ls())

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC") # Sets working directory

#### Functions ####

calcSpeeds<-function(Re){  # Calculates air speed from Reynolds number
  C=0.1
  nu=1.5e-05
  return((Re*nu)/C)
} 

calcMaxCLCD<-function(parameters,ARnumber,Cambernumber,Re){
  data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_0.csv",sep=""),header=TRUE)
  data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
  speed<-seq(2,30,by=0.5)
  data$speed<-speed
  data<-data[,c(17,3,4,1,2,5,6,7,8,9,10,11,12,13,14,15,16)]
  aoa<-seq(-2,5,by=1)
  CL<-rep(NA,length(aoa))
  CD<-rep(NA,length(aoa))
  for(i in 1:length(aoa)){
    CL[i]<-data[data$speed==10,2*i]
    CD[i]<-data[data$speed==10,2*i-1]
  }
  curve<-smooth.spline(y=CD,x=CL)
  new.curve<-predict(curve,seq(min(CL),max(CL),by=0.0001))
  slopes=new.curve$x/new.curve$y
  loweraoa=aoa[curve.data$CL==max(curve.data$CL[new.curve$x[slopes==max(slopes)]>curve.data$CL])]
  upperaoa=aoa[curve.data$CL==min(curve.data$CL[new.curve$x[slopes==max(slopes)]<curve.data$CL])]
  slopeaoa=(min(curve.data$CL[new.curve$x[slopes==max(slopes)]<curve.data$CL])-max(curve.data$CL[new.curve$x[slopes==max(slopes)]>curve.data$CL]))/(upperaoa-loweraoa)
  baoa=min(curve.data$CL[new.curve$x[slopes==max(slopes)]<curve.data$CL])-slopeaoa*upperaoa
  maxaoa=(new.curve$x[slopes==max(slopes)]-baoa)/slopeaoa
}

##### Loading Parameters ##### 

parameters<-read.table("input_data_681.dat",header=FALSE)  # Reads in parameter file for 681 simulations
names(parameters)<-c("AR","Camber","Re") # Sets names to columns

# Create factor data class columns for each parameter, record levels
parameters$ARFac<-as.factor(parameters$AR) 
levels.AR<-levels(parameters$ARFac)
parameters$CamberFac<-as.factor(parameters$Camber)
levels.Camber<-levels(parameters$CamberFac)
parameters$ReFac<-as.factor(parameters$Re)
levels.Re<-levels(parameters$ReFac)

summary(parameters) #Summarize data
write.csv(levels.Camber,file="camber_list.csv") # Writes list of camber factor values to csv files

