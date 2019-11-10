rm(list=ls())

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC") # Sets working directory

#### Functions ####

calcSpeeds<-function(Re){  # Calculates air speed from Reynolds number
  C=0.1
  nu=1.5e-05
  return((Re*nu)/C)
} 

MaxCLCD<-function(runs,Cambernumber){ # Calculates Max CLCD from graph 0 for each set of cambers
  h<-0
  vec<-unique(as.numeric(runs$ARFac))
  maxCLCD<-matrix(data=NA,ncol=2,nrow=nrow(runs))
  for(j in 1:length(vec)){
    ARnumber<-vec[j]
    runs2<-runs[runs$ARFac==levels.AR[ARnumber],]
    runs2<-runs2[order(as.character(runs2$speeds)),]
    data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_2.csv",sep=""),header=TRUE)
    data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
    for(i in 1:nrow(runs2)){
      h<-h+1
      x=na.omit(data[,2*i-1])
      y=na.omit(data[,2*i])
      curve<-smooth.spline(y=y,x=x,spar=0.5)
      new.curve<-predict(curve,seq(min(x),max(x),by=0.0001))
      maxCLCD[h,1]<-max(new.curve$y)
      maxCLCD[h,2]<-new.curve$x[new.curve$y==max(new.curve$y)]
    }
  }
  return(maxCLCD)
}

slopeCL<-function(runs,Cambernumber){ # Calculates slope and yint from graph 3
  h<-0
  vec<-unique(as.numeric(runs$ARFac))
  slopes<-matrix(data=NA,ncol=2,nrow=nrow(runs))
  colnames(slopes)<-c("slope","yint")
  for(j in 1:length(vec)){
    ARnumber<-vec[j]
    runs2<-runs[runs$ARFac==levels.AR[ARnumber],]
    runs2<-runs2[order(as.character(runs2$speeds)),]
    data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_3.csv",sep=""),header=TRUE)
    data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
    for(i in 1:nrow(runs2)){
      h<-h+1
      model<-lm(data[,2*i]~data[,2*i-1])
      slopes[h,1]<-as.numeric(model[["coefficients"]][2])
      slopes[h,2]<-as.numeric(model[["coefficients"]][1])
    }
  }
  return(slopes)
}

Efficiency<-function(runs,Cambernumber){ # Calculates Efficiency from graph 1
  h<-0
  vec<-unique(as.numeric(runs$ARFac))
  eff<-rep(NA,times=nrow(runs))
  for(j in 1:length(vec)){
    ARnumber<-vec[j]
    runs2<-runs[runs$ARFac==levels.AR[ARnumber],]
    runs2<-runs2[order(as.character(runs2$speeds)),]
    data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_1.csv",sep=""),header=TRUE)
    data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
    for(i in 1:nrow(runs2)){
      h<-h+1
      eff[h]<-mean(data[,2*i],na.rm=TRUE)
    }
  }
  return(eff)
}

GammaMin<-function(runs,Cambernumber){ # Calculates minimum glide angle and aoa at min glide angle from graph 2
  h<-0
  vec<-unique(as.numeric(runs$ARFac))
  gammas<-matrix(data=NA,ncol=2,nrow=nrow(runs))
  colnames(gammas)<-c("Gamma","aoa")
  for(j in 1:length(vec)){
    ARnumber<-vec[j]
    runs2<-runs[runs$ARFac==levels.AR[ARnumber],]
    runs2<-runs2[order(as.character(runs2$speeds)),]
    data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_0.csv",sep=""),header=TRUE)
    data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
    for(i in 1:nrow(runs2)){
      h<-h+1
      x=na.omit(data[,2*i-1])
      y=na.omit(data[,2*i])
      curve<-smooth.spline(y=y,x=x,spar=0.5)
      new.curve<-predict(curve,seq(min(x),max(x),by=0.0001))
      gammas[h,1]<-min(new.curve$y)
      gammas[h,2]<-new.curve$x[new.curve$y==min(new.curve$y)]
    }
  }
  return(gammas)
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
parameters$speeds<-calcSpeeds(parameters$Re)
parameters$ARnumber<-as.numeric(parameters$ARFac)
summary(parameters) #Summarize data
parameters<-parameters[order(as.character(parameters$CamberFac)),]

#### Allocating space in data frame ####
parameters$CLCD<-rep(NA,nrow(parameters))
parameters$Caoa<-rep(NA,nrow(parameters))
parameters$slope<-rep(NA,nrow(parameters))
parameters$yint<-rep(NA,nrow(parameters))
parameters$gamma<-rep(NA,nrow(parameters))
parameters$gaoa<-rep(NA,nrow(parameters))
parameters$Efficiency<-rep(NA,nrow(parameters))

#### Main analysis loop ####
n<-nlevels(parameters$CamberFac)
g1<-0
g2<-0
for(Cambernumber in 1:n){
  message("Camber level ",Cambernumber)
  g1<-g2+1
  runs<-parameters[parameters$Camber==levels.Camber[Cambernumber],]
  g2<-g1+(nrow(runs)-1)
  C<-MaxCLCD(runs,Cambernumber)
  parameters$CLCD[g1:g2]<-C[,1]
  parameters$Caoa[g1:g2]<-C[,2]
  s<-slopeCL(runs,Cambernumber)
  parameters$slope[g1:g2]<-s[,1]
  parameters$yint[g1:g2]<-s[,2]
  parameters$Efficiency[g1:g2]<-Efficiency(runs,Cambernumber)
  gam<-GammaMin(runs,Cambernumber)
  parameters$gamma[g1:g2]<-gam[,1]
  parameters$gaoa[g1:g2]<-gam[,2]
}

#### Checking and Saving Data ####
parameters<-parameters[order(as.numeric(row.names(parameters))),]
message("^*~.*^*~Completeness check~*^*~.~*^")
message("Number of NAs: ",sum(is.na(parameters)))

write.csv(parameters,file=paste("birdwing_gpc_data_",date(),".csv",sep=""))

library(ggplot2)
ggplot(parameters,aes(Camber,Re,color=gamma))+geom_point(size=3)+
  scale_color_gradient2(midpoint=mean(parameters$gamma), 
                         low="blue", mid="white",high="red", 
                         space ="Lab" )

ggplot(parameters,aes(Camber,Re,color=Caoa))+geom_point(size=3)+
  scale_color_gradient2(midpoint=mean(parameters$Caoa), 
                        low="blue", mid="white",high="red", 
                        space ="Lab" )


