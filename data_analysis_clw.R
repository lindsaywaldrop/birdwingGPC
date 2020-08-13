# Data Analysis script -- calculates output values from the raw simulation data inside panelwing folder
# To run this file: be sure to set your working directory to the main git directory.

rm(list=ls())

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC/") # Sets working directory

#### Functions ####

calcSpeeds<-function(Re){  # Calculates air speed from Reynolds number
  C=0.1
  nu=1.5e-05
  return((Re*nu)/C)
} 

findL<-function(AR){AR/18.06685}


VzMin<-function(runs,Cambernumber){ # Calculates minimum glide angle and aoa at min glide angle from graph 2
  h<-0
  vec<-unique(as.numeric(runs$ARFac))
  Vzs<-matrix(data=NA,ncol=2,nrow=nrow(runs))
  colnames(Vzs)<-c("Vz","aoa")
  for(j in 1:length(vec)){
    ARnumber<-vec[j]
    message("ARnumber",ARnumber)
    runs2<-runs[runs$ARFac==levels.AR[ARnumber],]
    runs2<-runs2[order(as.character(runs2$speeds)),]
    data<-read.csv(paste("Cam",Cambernumber,"/AR",ARnumber,"_Graph_1.csv",sep=""),header=TRUE)
    data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
    for(i in 1:nrow(runs2)){
      h<-h+1
      x=na.omit(data[,2*i-1])
      y=na.omit(data[,2*i])
      curve<-smooth.spline(y=y,x=x,spar=0.5)
      new.curve<-predict(curve,seq(min(x),max(x),by=0.0001))
      Vzs[h,1]<-min(new.curve$y)
      Vzs[h,2]<-new.curve$x[new.curve$y==min(new.curve$y)]
    }
  }
  return(Vzs)
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
parameters$Lengths<-findL(parameters$AR)
parameters$ARnumber<-as.numeric(parameters$ARFac)
summary(parameters) #Summarize data
parameters<-parameters[order(as.character(parameters$CamberFac)),]

# #### For running gpc ####
runs<-parameters[parameters$Camber==levels.Camber[5],]
nrow(parameters[parameters$Camber==levels.Camber[9],])

progress<-18
for(i in 1:8){
  progress<-progress+nrow(parameters[parameters$Camber==levels.Camber[i],])
}
progress/681
# 

#### Allocating space in data frame ####
parameters$Vz<-rep(NA,nrow(parameters))
parameters$Vzaoa<-rep(NA,nrow(parameters))

#### Main analysis loop ####
base<-getwd()
setwd(paste(base,"/panelwingGPC_cwl",sep="")) # Sets working directory

n<-nlevels(parameters$CamberFac)
g1<-0
g2<-0
for(Cambernumber in 1:n){
  message("Camber level ",Cambernumber)
  g1<-g2+1
  runs<-parameters[parameters$Camber==levels.Camber[Cambernumber],]
  g2<-g1+(nrow(runs)-1)
  vz<-VzMin(runs,Cambernumber)
  parameters$Vz[g1:g2]<-vz[,1]
  parameters$Vzaoa[g1:g2]<-vz[,2]
}

  
#### Checking and Saving Data ####
parameters<-parameters[order(as.numeric(row.names(parameters))),]
message("~.*^*~Completeness check~*^*~.~\n",
        "Number of NAs: ",sum(is.na(parameters)))


write.csv(parameters,file=paste("birdwing_gpc_data_",date(),".csv",sep=""))

  

