#Plotting script

#### Required Libraries ####
library(ggplot2)
library(scatterplot3d)
library(viridis)
#### Functions ####

calcRe<-function(S,C){  # Calculates air speed from Reynolds number
  nu=1.5e-05
  return((S*C)/nu)
} 

# Creates different color values for individual points.
myColorRamp <- function(colors, values, minmax) { 
  v <- (values - min(minmax))/diff(range(minmax))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

# Creates base scatterplot3d plot with selected parameters (2d domain),
# and an output value (z-axis). Data should be data frame containing all values.
# Parameters and values should be name of column
# e.g. "Camber" or "Caoa" as character strings. To add to plot, call
# this function as gpc3d.object<-gpc3d(...)
gpc3d<-function(data,parameter1,parameter2,value,alpha,colors){
  par1<-data[,which(names(data)==parameter1)]
  par2<-data[,which(names(data)==parameter2)]
  val<-data[,which(names(data)==value)]
  cols <- myColorRamp(colors, val, range(val))
  gpc3d.object<-scatterplot3d(par1,par2,val,
                              pch=16,color=cols,
                              xlab=parameter1,
                              #ylab=" ",
                              ylab=parameter2,
                              zlab=value,
                              angle=alpha,y.margin.add=0.5)
  #dims <- par("usr")
  #x <- dims[1]+ xadj*diff(dims[1:2])
  #y <- dims[3]+ yadj*diff(dims[3:4])
  #text(x,y,parameter2,srt=alpha,font=1,cex=1.0)
  return(gpc3d.object)
}

# Adds onto plot made by gpc3d using gpc3d.object. Data should be data frame 
# with data to add to plot, parameters and values similar to above. 
birdsadd3D<-function(gpc3d.object,data,parameter1,parameter2,value){
  par1<-data[,which(names(data)==parameter1)]
  par2<-data[,which(names(data)==parameter2)]
  val<-data[,which(names(data)==value)]
  gpc3d.object$points3d(par1,par2,val,
                        pch=23)
  p.coords<-gpc3d.object$xyz.convert(par1,par2,val)
  text(p.coords$x,p.coords$y,p.coords$z,labels=data[,1],pos=sample(1:4,replace=TRUE),cex=0.6)
}

# 
gpc3d2<-function(data,parameter1,parameter2,parameter3,value,alpha,xadj,yadj,colors){
  par1<-data[,which(names(data)==parameter1)]
  par2<-data[,which(names(data)==parameter2)]
  par3<-data[,which(names(data)==parameter3)]
  val<-data[,which(names(data)==value)]
  cols <- myColorRamp(colors, val, range(val))
  gpc3d.object<-scatterplot3d(par1,par2,par3,
                              pch=16,color=cols,
                              xlab=parameter1,
                              ylab=" ",
                              #ylab=parameter2,
                              zlab=parameter3,
                              angle=alpha,y.margin.add=1)
  dims <- par("usr")
  x <- dims[1]+ xadj*diff(dims[1:2])
  y <- dims[3]+ yadj*diff(dims[3:4])
  text(x,y,parameter2,srt=alpha,font=1,cex=1.0)
  return(gpc3d.object)
}

point2dplot<-function(gpcdata,speciesdata,parameter,colors){
  gpcpar<-gpcdata[,which(names(gpcdata)==parameter)]
  speciespar<-speciesdata[,which(names(speciesdata)==parameter)]
  combined<-c(gpcpar,speciespar)
  ggplot(gpcdata,aes(Camber,AR,color=get(parameter)))+geom_point(size=3)+
    scale_color_gradient2(midpoint=mean(combined), 
                          limits=range(combined),
                          low=colors[1], mid=colors[2],high=colors[3], 
                          space ="Lab", guide=FALSE) +
    geom_point(data=speciesdata,mapping=aes(x=Median.Camber,
                                        y=Median.Aspect.Ratio,
                                        fill=get(parameter)),color="black",shape=21,size=3) +
    scale_fill_gradient2(midpoint=mean(combined), 
                         limits=range(combined),
                         low=colors[1], mid=colors[2],high=colors[3],
                         space ="Lab") +
    labs(fill=parameter)
}



#### Loading Data ####

# Load the gpc data csv in the birdwingGPC/panelwingGPC folder
parameters<-read.csv(file=file.choose(),header=TRUE) 
parameters$slopeyint<-parameters$slope+parameters$yint
# Load the species data csv in the Species_models/summary_data folder
species<-read.csv(file=file.choose(),header=TRUE)    
species$slopeyint<-species$slope+species$yint

data<-read.csv("Combined_birdwing_gpc_data_cwl.csv")

#### 3D Scatterplots ####

w=5.25
h=5
# CLCD
i=85
for (i in seq(0,180,by=1)){
  #pdf(file = paste("animCLCD/CLCD",i,".pdf",sep=""))
  png(filename=paste("animCLCD/CLCD",i,".png",sep=""),width=w,height=h,unit="in",res=200)
  gpc3d.object<-gpc3d(parameters,"Camber","AR","maxL.Vzaoa",i,c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))
  gpc3d.object<-gpc3d(parameters,"AR","Camber","maxL.5aoa",i,c("steelblue","red"))
  #birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","CLCD")
  gpc3d.object
  dev.off()
}

gpc3d2(parameters,"Camber","Re","AR","CLCD",55,0.8,0.1,c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))


# Max Angle of Attack
for (i in seq(0,180,by=10)){
  pdf(file = paste("animCaoa/Caoa",i,".pdf",sep=""))
  gpc3d.object<-gpc3d(parameters,"Camber","AR","Caoa",i,c("blue","purple","red"))
  birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","Caoa")
  dev.off()
}

gpc3d2(parameters,"Camber","Re","AR","Caoa",55,0.8,0.1,c("yellow","orange","red"))

# Gliding angle
for (i in seq(0,180,by=10)){
  pdf(file = paste("animgamma/gamma",i,".pdf",sep=""))
  gpc3d.object<-gpc3d(parameters,"Camber","AR","gamma",i,c("yellow","orange","red"))
  birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","gamma")
  dev.off()
}


gpc3d2(data,"Camber","Re","AR","Vz",55,0.8,0.1,c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))
gpc3d2(data,"Camber","Re","AR","Vz.cwl",55,0.8,0.1,c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))
# Efficiency
for (i in seq(0,180,by=10)){
  pdf(file = paste("animEfficiency/Efficiency",i,".pdf",sep=""))
  gpc3d.object<-gpc3d(parameters,"Camber","AR","Efficiency",i,c("yellow","orange","red"))
  birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","Efficiency")
  dev.off()
}

gpc3d2(parameters,"Camber","Re","AR","Efficiency",55,0.8,0.1,c("yellow","orange","red"))


# Slope 
for (i in seq(0,180,by=1)){
  png(filename=paste("animslope/slope",i,".png",sep=""),width=w,height=h,unit="in",res=200)
  gpc3d.object<-gpc3d(parameters,"Camber","AR","slope",i,c("darkred","white"))
  #birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","slope")
  dev.off()
}


# Slope + Yint
for (i in seq(0,180,by=10)){
  pdf(file = paste("animslopeyint/slopeyint",i,".pdf",sep=""))
  gpc3d.object<-gpc3d(parameters,"Camber","AR","slopeyint",i,c("yellow","orange","red"))
  birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","slopeyint")
  dev.off()
}

gpc3d2(parameters,"Camber","Re","AR","slope",55,0.8,0.1,c("yellow","orange","red"))
gpc3d2(parameters,"Camber","Re","AR","CLCD",55,0.8,0.1,c("blue","blue"))
gpc3d(parameters,"Camber","AR","yint",70,c("yellow","orange","red"))

#### GGPLOTS ####

point2dplot(parameters,species,"gamma",c("blue","grey","red"))
point2dplot(parameters,species,"Vz",c("yellow","orange","red"))
point2dplot(parameters,species,"Caoa",c("blue","white","red"))
point2dplot(parameters,species,"Efficiency",c("blue","white","red"))
point2dplot(parameters,species,"slopeyint",c("blue","white","red"))

#### Other Trash ####
 
cols <- myColorRamp(c("blue","green","yellow","red"), parameters$Vz, range(parameters$Vz))
cols <- myColorRamp(c("blue","green","yellow","red"), parameters$maxL.Vzaoa, range(parameters$maxL.Vzaoa))
cols <- myColorRamp(c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"), parameters$maxL.Vzaoa, range(parameters$maxL.Vzaoa))

plot(parameters$AR,parameters$Camber,col=cols,pch=16)
scols<-myColorRamp(c("blue","red"), species$CLCD, range(parameters$CLCD))
points(species$Median.Aspect.Ratio,species$Median.Camber,bg=scols,col=NA,pch=23)

points(species$Median.Aspect.Ratio[species$type_list=="raptor"],
       species$Median.Camber[species$type_list=="raptor"],
       col="orange",pch=23)

points(species$Median.Aspect.Ratio[species$type_list=="song"],
       species$Median.Camber[species$type_list=="song"],
       col="black",pch=23)

points(species$Median.Aspect.Ratio[species$species_list=="Cathartes_aura"],
       species$Median.Camber[species$species_list=="Cathartes_aura"],
       col="orange",pch=23,cex=2)

albatross<-data.frame(Camber=0.10484,AR=11.5)
points(albatross$AR,albatross$Camber,pch=23,cex=2)


ggplot(parameters,aes(AR,Camber,color=CLCD))+geom_point(size=3)+
  scale_color_gradient2(midpoint=mean(parameters$CLCD), 
                        limits=range(parameters$CLCD),
                        low=colors[1], mid=colors[2],high=colors[3], 
                        space ="Lab", guide=FALSE)




par(mfrow=c(1,2))
gpc3d2(data,"Camber","Re","AR","Vz",55,0.8,0.1,c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))
gpc3d2(data,"Camber","Re","AR","Vz.cwl",55,0.8,0.1,c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))


color.bar <- function(colors, values, minmax, nticks,digits) {
  #min=round(min(values),digits=digits)
  min=min(minmax)
  tickmin=min
  #max=round(max(values),digits=digits)
  max=max(minmax)
  ticks=seq(tickmin, max, len=nticks)
  #	ticks<-formatC(ticks,format="e",digits=1)
  ticks<-formatC(ticks,digits=digits)
  scale<-seq(from=min,to=max,length.out=100)
  len<-(length(scale)-1)/(max-min)
  x <- myColorRamp(colors,scale,minmax)
  par(mar=c(1,2,1,1))
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='')
  #    axis(2, ticks, las=1,cex.axis=0.8)
  axis(2, at=ticks,labels=ticks, las=1,cex.axis=1) #for scientific formating of labels
  for (i in 1:(length(scale)-1)) {
    y = (i-1)/len + min
    rect(0,y,10,y+1/len, col=x[i], border=NA)
  }
}

layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),widths=c(4,4,1), heights=c(1))
colors<-c("#440154FF","#31688EFF","#35B779FF","#FDE725FF")
cols1 <- myColorRamp(colors, data$Vz, range(data$Vz.cwl))
scatterplot3d(data$Camber,data$Re,data$AR,
              pch=16,cex.symbols = 2,color=cols1,
              xlab="Camber",
              ylab=" ",
              zlab="AR",
              main="Vz constant mass",
              angle=55,y.margin.add=1)
dims <- par("usr")
x <- dims[1]+ 0.7*diff(dims[1:2])
y <- dims[3]+ 0.1*diff(dims[3:4])
text(x,y,"Re",srt=55,font=1,cex=2.0)

cols2 <- myColorRamp(colors, data$Vz.cwl, range(data$Vz.cwl))
scatterplot3d(data$Camber,data$Re,data$AR,
              pch=16,cex.symbols=2,color=cols2,
              xlab="Camber",
              ylab=" ",
              zlab="AR",
              main="Vz constant wing loading",
              angle=55,y.margin.add=1)
dims <- par("usr")
x <- dims[1]+ 0.7*diff(dims[1:2])
y <- dims[3]+ 0.1*diff(dims[3:4])
text(x,y,"Re",srt=55,font=1,cex=2.0)

color.bar(colors<-c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"),data$Vz,range(data$Vz.cwl),5,2)


