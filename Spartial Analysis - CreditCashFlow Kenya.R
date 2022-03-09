#SPACIAL DATA (FINAL) ASSIGNMENT
#The task is to pull out each county and show the spread of Credit_level and CashFlow_Level in each county. 
#I.e. Mombasa county will generate two maps of Mombasa; one that shows Credit_level and another CashFlow_Level.
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
Kenya2<-getData("GADM", country = 'KE', level = 2)   #sub-counties
plot(Kenya2)

#DATASET BELOW (Retrieving data for each county)
library(readxl)
OOP_Final_Assignment <- read_excel("F:/Classwork/Year I/Year I Sem II/Object Oriented Programming/Data/OOP_Final Assignment.xlsx", sheet = "Sheet1")
head(OOP_Final_Assignment)
class(OOP_Final_Assignment)

Mombasa<-OOP_Final_Assignment[1:6,]
Mombasa_cl<-cbind(Mombasa$Constituencies, Mombasa$Credit_level)
Mombasa_cfl<-cbind(Mombasa$Constituencies,Mombasa$CashFlow_Level)

Kilifi<-OOP_Final_Assignment[7:13,]
Kilifi_cl<-cbind(Kilifi$Constituencies, Kilifi$Credit_level)
Kilifi_cfl<-cbind(Kilifi$Constituencies,Kilifi$CashFlow_Level)

Meru<-OOP_Final_Assignment[14:22,]
Meru_cl<-cbind(Meru$Constituencies, Meru$Credit_level)
Meru_cfl<-cbind(Meru$Constituencies,Meru$CashFlow_Level)

Kitui<-OOP_Final_Assignment[23:30,]
Kitui_cl<-cbind(Kitui$Constituencies, Kitui$Credit_level)
Kitui_cfl<-cbind(Kitui$Constituencies,Kitui$CashFlow_Level)

Machakos<-OOP_Final_Assignment[31:38,]
Machakos_cl<-cbind(Machakos$Constituencies, Machakos$Credit_level)
Machakos_cfl<-cbind(Machakos$Constituencies,Machakos$CashFlow_Level)

Nairobi<-OOP_Final_Assignment[39:55,]
Nairobi_cl<-cbind(Nairobi$Constituencies, Nairobi$Credit_level)
Nairobi_cfl<-cbind(Nairobi$Constituencies,Nairobi$CashFlow_Level)

#CREDIT LEVEL DATA
creditlevel<-rbind(Mombasa_cl, Kilifi_cl, Meru_cl, Kitui_cl, Machakos_cl, Nairobi_cl)
creditlevel<-as.data.frame(creditlevel)
colnames(creditlevel)<-c("NAME_2","CreditLevel")

class(creditlevel$NAME_2)         #class factor
creditlevel$NAME_2<-as.character(creditlevel$NAME_2)

#CASH FLOW LEVEL DATA
cashflowlevel<-rbind(Mombasa_cfl, Kilifi_cfl, Meru_cfl, Kitui_cfl, Machakos_cfl, Nairobi_cfl)
cashflowlevel<-as.data.frame(cashflowlevel)
colnames(cashflowlevel)<-c("NAME_2","CashflowLevel")

class(cashflowlevel$NAME_2)         #class factor
cashflowlevel$NAME_2<-as.character(cashflowlevel$NAME_2)


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
Kenya2_UTM<-spTransform(Kenya2,CRS("+init=EPSG:32737"))  #to get names of counties, subcounties and constituencies
Kenya2_UTM@data$NAME_2           

class(Kenya2_UTM@data$NAME_2)     #class character
Kenya2_UTM@data                   #length 255

NAME_2<-Kenya2_UTM@data$NAME_2    #names of counties, subcounties and constituencies
#class character

#GENERATING THE FINAL DATA
library(plyr)
library(dplyr)

Kenya2_UTM@data$id<-rownames(Kenya2_UTM@data)
class(Kenya2_UTM@data$id)         #class character

Kenya2_UTM@data<-inner_join(Kenya2_UTM@data, creditlevel, by="NAME_2")  #length 54
Kenya2_UTM@data<-inner_join(Kenya2_UTM@data, cashflowlevel, by="NAME_2")#length 54

Kenya2_final<-fortify(Kenya2_UTM)  #length 817294+
View(Kenya2_final)
Kenya2_final<-inner_join(Kenya2_final, Kenya2_UTM@data, by = 'id')

class(Kenya2_final$CashflowLevel)
Kenya2_final$CashflowLevel<-as.numeric(Kenya2_final$CashflowLevel)
Kenya2_final$CreditLevel<-as.numeric(Kenya2_final$CreditLevel)

#PLOTS
#Credit Level
ggplot()+
  geom_polygon(data = Kenya2_final, aes(x=long, y=lat, group=group, fill = CreditLevel), colour = "black", size = 0.25)+
  theme(aspect.ratio = 1,
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank())+ 
  scale_fill_distiller(name = "CreditLevel", palette = "Reds", trans = "reverse", breaks = pretty_breaks(n=4))+
  labs(title = "Credit Level  in Selected Counties in Kenya")

#Cash Flow Level
ggplot()+
  geom_polygon(data = Kenya2_final, aes(x=long, y=lat, group=group, fill = CashflowLevel), colour = "black", size = 0.25)+
  theme(aspect.ratio = 1,
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank())+ 
  scale_fill_distiller(name = "CashFlowLevel", palette = "Greens", trans = "reverse", breaks = pretty_breaks(n=4))+
  labs(title = "Cash Flow Level  in Selected Counties in Kenya")
