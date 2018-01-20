#Name: Project 4
#Authoer: Juan P. Garces
#Date: 10/10/2016

library(dplyr)
library(tidyr)
DATA1 <- readRDS("summarySCC_PM25.rds")
DATA2 <- readRDS("Source_Classification_Code.rds")

#Question 1 - Creates two dataframes, for later use (Explained in Comment 3), Filters and Selects the data
MAIND <- DATA1 %>% filter(fips=="06037" | fips=="06059" | fips=="06071")
DATA11 <- MAIND %>% select(fips, SCC, Emissions) 
str(DATA11)

#Question 2 - Created two "DataFrame Filters" to get the data I needed
CountyNames <- data.frame(fips = c("06037", "06059", "06071"), County = c("Los Angeles County","Orange County","San Bernadino County"))
Solvents <- data.frame(Solvent = c("Yes"), EI.Sector= c("Solvent"), stringsAsFactors=FALSE)

DATA11 <- DATA11 %>% left_join(CountyNames, by="fips") %>% left_join(DATA2, by="SCC") %>% 
  mutate(EI.Sector= substr(EI.Sector,1,7)) %>% left_join(Solvents, by="EI.Sector") %>% #Used EI.Sector to find Solvent Emissions
  select(fips, SCC, Emissions, County, Solvent)
  DATA11$Solvent[which(is.na(DATA11$Solvent))] <- "No" #Because it only looks for the Solvent ones, It fills the other values with "No"
str(DATA11)

#3 Used the Dataframe from the First part of the program to efficiently join both dataframes (I tried using DATA1 and it TAKES A LONG TIME)
DATA11 <- DATA11 %>% left_join(select(MAIND, year, SCC), by="SCC") %>% 
  select(County, Emissions, Solvent, year) %>% 
  filter(year > 1999) #All Years except 1999
str(DATA11)

#------------Not Much Comments Summarizing is pretty straight forward ------------------------------------
#Question Summarizing 4
DATA4 <- DATA11 %>% group_by(Solvent, County, year) %>% summarise(Total = sum(Emissions))
DATA4
#Question Summarizing 5
DATA5 <- DATA11 %>% group_by(Solvent, County) %>% summarise(Total = sum(Emissions))
DATA5
#Question Summarizing 6
DATA6 <- DATA11 %>% filter(Solvent == "Yes") %>% group_by(year) %>% summarise(Total = sum(Emissions))
DATA6
#Question Summarizing 7
DATA7 <- DATA11 %>% filter(year == 2008) %>% group_by(County) %>% summarise(Total = sum(Emissions))
DATA7
#Question Summarizing 8
DATA8 <- DATA11 %>% summarise(Total = sum(Emissions))
DATA8
