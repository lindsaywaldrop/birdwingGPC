rm(list=ls())

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC") # Sets working directory

#### Functions ####

calcSpeeds<-function(Re){  # Calculates air speed from Reynolds number
  C=0.1
  nu=1.5e-05
  return((Re*nu)/C)
} 

MaxCLCD<-function(runs2,levels.AR,ARnumber,Cambernumber){
  print(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_2.csv",sep=""))
  data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_2.csv",sep=""),header=TRUE)
  data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
  speeds<-calcSpeeds(as.numeric(as.character(runs2[,6])))
  runs2<-runs2[order(as.character(speeds)),]
  maxCLCD<-rep(NA,nrow(runs2))
  for(i in 1:nrow(runs2)){
    x=na.omit(data[,2*i-1])
    y=na.omit(data[,2*i])
    curve<-smooth.spline(y=y,x=x)
    new.curve<-predict(curve,seq(min(x),max(x),by=0.0001))
    maxCLCD[i]<-max(new.curve$y)
  }
  return(maxCLCD)
}

slopeCL<-function(runs2,levels.AR,ARnumber,Cambernumber){
  data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_3.csv",sep=""),header=TRUE)
  data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
  speeds<-calcSpeeds(as.numeric(as.character(runs2[,6])))
  runs2<-runs2[order(as.character(speeds)),]
  slopeCL<-rep(NA,nrow(runs2))
  yintCL<-rep(NA,nrow(runs2))
  for(i in 1:nrow(runs2)){
    model<-lm(data[,2*i]~data[,2*i-1])
    slopeCL[i]<-as.numeric(model[["coefficients"]][2])
    yintCL[i]<-as.numeric(model[["coefficients"]][1])
  }
  CL<-data.frame(slopeCL,yintCL)
  return(CL)
}

Efficiency<-function(runs2,levels.AR,ARnumber,Cambernumber){
  data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_1.csv",sep=""),header=TRUE)
  data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
  speeds<-calcSpeeds(as.numeric(as.character(runs2[,6])))
  runs2<-runs2[order(as.character(speeds)),]
  eff<-rep(NA,nrow(runs2))
  for(i in 1:nrow(runs2)){
    eff[i]<-mean(data[,2*i],na.rm=TRUE)
  }
  return(eff)
}

GammaMin<-function(runs2,levels.AR,ARnumber,Cambernumber){
  data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_0.csv",sep=""),header=TRUE)
  data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
  speeds<-calcSpeeds(as.numeric(as.character(runs2[,6])))
  runs2<-runs2[order(as.character(speeds)),]
  Gamma<-rep(NA,nrow(runs2))
  aoa<-rep(NA,nrow(runs2))
  for(i in 1:nrow(runs2)){
    x=na.omit(data[,2*i-1])
    y=na.omit(data[,2*i])
    curve<-smooth.spline(y=y,x=x)
    new.curve<-predict(curve,seq(min(x),max(x),by=0.0001))
    Gamma[i]<-min(new.curve$y)
    aoa[i]<-new.curve$x[new.curve$y==min(new.curve$y)]
  }
  G<-data.frame(Gamma,aoa)
  return(G)
}


ARnumber<-13
Cambernumber<-2
runs<-parameters[parameters$Camber==levels.Camber[4],]
levels.AR<-levels(parameters$ARFac)
runs2<-runs[runs$ARFac==levels.AR[ARnumber],]

m<-MaxCLCD(runs2,levels.AR,13,2)
m<-MaxCLCD(runs2,levels.AR,8,2)
m<-MaxCLCD(runs2,levels.AR,18,2)
m<-slopeCL(runs2,levels.AR,13,2)
eff<-Efficiency(runs2,levels.AR,13,2)
g<-GammaMin(runs2,levels.AR,13,2)

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
parameters$speeds<-calcSpeeds(parameters$Re)
parameters$ARnumber<-as.numeric(parameters$ARFac)

summary(parameters) #Summarize data
write.csv(levels.Camber,file="camber_list.csv") # Writes list of camber factor values to csv files

