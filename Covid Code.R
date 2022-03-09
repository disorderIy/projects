#KENYA
#COVID-19 CASES BY COUNTY
#libraries
library(sp)
library(dplyr)
library(ggplot2)
library(gdata)
library(gtools)
library(RgoogleMaps)
library(ggmap)
library(maptools)
library(raster)
library(rgdal)
library(scales)

#sPACIAL DATA BELOW (Contains the plots)
Kenya<-getData("GADM", country = 'KE', level = 0)
plot(Kenya)

Kenya1<-getData("GADM", country = 'KE', level = 1)   #counties
plot(Kenya1)

#Kenya2<-getData("GADM", country = 'KE', level = 2)   #sub-counties
#plot(Kenya2)

#DATASET BELOW (Retrieving data for each county)
#library(readxl)
Covid <- read.csv("F:/Classwork/Year I/Year I Sem II/Object Oriented Programming/Data/countydata.csv", header = TRUE)
head(Covid)
class(Covid)  #data.frame

#COVID-19 PATIENTS DATA
#Covid<-as.data.frame(Covid)
colnames(Covid)<-c("NAME_1","Cases")
class(Covid$Cases)         #class factor
Covid$NAME_1 <- as.character(Covid$NAME_1)
Covid$Cases <-as.numeric(Covid$Cases)

theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(), 
                       axis.text.y = element_blank(), 
                       axis.ticks = element_blank(), 
                       axis.title.x = element_blank(), 
                       axis.title.y = element_blank(), 
                       plot.title = element_blank()))
#sets the background for the plot

#SPACIAL BIT
Kenya1_UTM<-spTransform(Kenya1,CRS("+init=EPSG:32737"))  #to get names of counties
Kenya1_UTM@data$NAME_1      #output is a list of county names
class(Kenya1_UTM@data$NAME_1)   #class character

NAME_1<-Kenya1_UTM@data$NAME_1    #names of counties
#class character

#GENERATING THE FINAL DATA
library(plyr)
library(dplyr)

Kenya1_UTM@data$id<-rownames(Kenya1_UTM@data)   #1...47
class(Kenya1_UTM@data$id)         #class character

Kenya1_UTM@data<-inner_join(Kenya1_UTM@data, Covid, by="NAME_1")  #length 47

Kenya1_final<-fortify(Kenya1_UTM) 
#View(Kenya1_final)
Kenya1_final<-inner_join(Kenya1_final, Kenya1_UTM@data, by = 'id')

class(Kenya1_final$Cases)
#PLOTS
ggplot()+
  geom_polygon(data = Kenya1_final, aes(x=long, y=lat, group=group, fill = Cases), colour = "black", size = 0.25)+
  theme(aspect.ratio = 1,
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank())+ 
  scale_fill_distiller(name = "Number of Cases", palette = "Reds", trans = "reverse", breaks = pretty_breaks(n=8))+
  labs(title = "COVID-19 Cases Per County in Kenya as at June 2021")

