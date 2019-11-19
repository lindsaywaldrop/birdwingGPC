#Plotting script

#### Required Libraries ####
library(ggplot2)
library(scatterplot3d)

#### Functions ####

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
                              xlab=parameter1,ylab=parameter2,
                              zlab=value,
                              angle=alpha)
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
}


#### Loading Data ####

# Load the gpc data csv in the birdwingGPC/panelwingGPC folder
parameters<-read.csv(file=file.choose(),header=TRUE) 
parameters$slopeyint<-parameters$slope+parameters$yint
# Load the species data csv in the Species_models/summary_data folder
species<-read.csv(file=file.choose(),header=TRUE)    
species$slopeyint<-species$slope+species$yint

#### 3D Scatterplots ####

# CLCD
gpc3d.object<-gpc3d(parameters,"Camber","AR","CLCD",155,c("yellow","orange","red"))
birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","CLCD")

# Max Angle of Attack
gpc3d.object<-gpc3d(parameters,"Camber","AR","Caoa",5,c("blue","purple","red"))
birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","Caoa")

# Gliding angle
gpc3d.object<-gpc3d(parameters,"Camber","AR","gamma",55,c("yellow","orange","red"))
birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","gamma")

# Efficiency
gpc3d.object<-gpc3d(parameters,"Camber","AR","Efficiency",0,c("yellow","orange","red"))
birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","Efficiency")

# Slope + Yint
gpc3d.object<-gpc3d(parameters,"Camber","AR","slopeyint",90,c("yellow","orange","red"))
birdsadd3D(gpc3d.object,species,"Median.Camber","Median.Aspect.Ratio","slopeyint")


#### GGPLOTS ####

ggplot(parameters,aes(Camber,Re,color=gamma))+geom_point(size=3)+
  scale_color_gradient2(midpoint=mean(parameters$gamma), 
                        low="blue", mid="white",high="red", 
                        space ="Lab" )

ggplot(parameters,aes(Camber,AR,color=CLCD))+geom_point(size=3)+
  scale_color_gradient2(midpoint=mean(parameters$Caoa), 
                        low="blue", mid="white",high="red", 
                        space ="Lab" )





cols <- myColorRamp(c("blue","red"), parameters$slope+parameters$yint, range(parameters$slope+parameters$yint))
plot(parameters$Camber,parameters$AR,col=cols,pch=16)
scols<-myColorRamp(c("blue","red"), species$slope+species$yint, range(parameters$slope+parameters$yint))
points(species$Median.Camber,species$Median.Aspect.Ratio,bg=scols,col=NA,pch=23)

points(species$Median.Camber[species$type_list=="raptor"],
       species$Median.Aspect.Ratio[species$type_list=="raptor"],
       col="orange",pch=23)

points(species$Median.Camber[species$type_list=="song"],
       species$Median.Aspect.Ratio[species$type_list=="song"],
       col="yellow",pch=23)

points(species$Median.Camber[species$species_list=="Cathartes_aura"],
       species$Median.Aspect.Ratio[species$species_list=="Cathartes_aura"],
       col="orange",pch=23,cex=2)

albatross<-data.frame(Camber=0.10484,AR=11.5)
points(albatross$Camber,albatross$AR,pch=23,cex=2)
