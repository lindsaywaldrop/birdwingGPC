
rm(list=ls())

setwd("~/Dropbox (Chapman)/wing_gPC/birdwingGPC")

parameters<-read.table("input_data_681.dat",header=FALSE)
names(parameters)<-c("AR","Camber","Re")
parameters$ARFac<-as.factor(parameters$AR)
levels.AR<-levels(parameters$ARFac)
parameters$CamberFac<-as.factor(parameters$Camber)
levels.Camber<-levels(parameters$CamberFac)
parameters$ReFac<-as.factor(parameters$Re)
levels.Re<-levels(parameters$ReFac)

summary(parameters)

write.csv(levels.Camber,file="camber_list.csv")

ASwing<-read.csv("AS6091.csv",header=TRUE)
camber.original<-(max(ASwing$y)-min(ASwing$y))/(max(ASwing$x)-min(ASwing$x))

sink("AS6091_base.dat")
cat("AS6091\ camber=",camber,"\n")
sink()
#write.table(c("wing"),file="birdwing.dat",append=FALSE,col.names=FALSE,row.names=FALSE,sep="  ")
write.table(ASwing,file="AS6091_base.dat",append=TRUE,col.names=FALSE,row.names=FALSE,sep="  ")

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


calcAR<-function(AR.new,C){0.5*AR.new*C}
calcSpeeds<-function(Re,C,nu){(Re*nu)/C}
calcAR(as.numeric(as.character(levels.AR[1])),0.1)
nu=1.5e-05
calcSpeeds(as.numeric(as.character(levels.Re[1])),0.1,nu)

parameters[parameters$Camber==levels.Camber[11],]
nrow(parameters[parameters$Camber==levels.Camber[11],])

progress<-0
for(i in 1:12){
progress<-progress+nrow(parameters[parameters$Camber==levels.Camber[i],])
}
progress/681
  
calcAR(10.098076,0.1)