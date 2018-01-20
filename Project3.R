#Name: Project 3
#Authoer: Juan P. Garces
#Date: 10/04/2016
library(ggplot2)
DATA <- readRDS("summarySCC_PM25.rds")

#For all the plots, I used bargraphs and calculating the Mean of the Emissions for accurate presentation
#Plot 1 All Emmissions
P1<- ggplot(DATA, aes(year, Emissions))+
  geom_bar(stat="summary", fun.y="mean", aes(fill=year))+
  ggtitle("Total Emissions Decresed in the U.S. 1999-2008")
print(P1)

#Filter for Baltimore City + Plot 2
P2Filter <- DATA$fips=="24510"
P2<- ggplot(DATA[P2Filter,], aes(year,Emissions))+ 
  geom_bar(stat="summary", fun.y="mean", aes(fill=year))+ 
  ggtitle("Total Emissions Decresed in Baltimore city, Maryland 1999-2008")
print(P2)

#Plot 3 with facet_grid for better presentation
P3<- ggplot(DATA[P2Filter,], aes(year,Emissions))+ 
  facet_grid(.~type)+
  geom_bar(stat="summary", fun.y="mean", aes(fill=type))+ 
  ggtitle("Total Emissions Types in the U.S. 1999-2008")
print(P3)

#Plot 4 + Filter getting on-road for Baltimore city and Los Angeles County
P32Filter<- DATA[ which( (DATA$type=="ON-ROAD" & DATA$fips=="06037") | (DATA$type=="ON-ROAD" & DATA$fips=="24510")) , ]
P4<- ggplot(P32Filter, aes(year, Emissions))+ 
  facet_grid(.~fips)+
  geom_bar(stat="summary", fun.y="mean",aes(fill=fips))+ 
  labs(fill="Cities")+
  ggtitle("Total On-road Emissions between Los Angeles County, California and Baltimore City, Maryland")
print(P4)

