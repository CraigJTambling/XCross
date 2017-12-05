rm(list=ls())
library(raster)    
library(rgdal)
library(aspace)
library(MuMIn)
library(adehabitatMA)
library(rgeos)
library(AICcmodavg)
library(maptools)

# Step 1 - load sighting data with GPS points and date and time and species
setwd("c:/Varsity Work/Projects/Camera Traps/Nyathi - 2013")
sight <- read.csv("Daysall.csv",header=T)

# Step 2 - Convert sighting data time to the correct format
time.sight <- as.POSIXct(strptime(
  paste(sight$Date), 
  format="%Y-%m-%d"),  tz="Africa/Johannesburg")
sight2 <- data.frame(sight,time.sight)

# Step 3 - Convert sighting data GPS coordinates to correct format
proj.sight <- project(as.matrix(sight[,c("Longitude","Latitude")]),"+proj=utm +south +zone=36 +ellps=WGS84")
sight3 <- data.frame(sight2,proj.sight)

# Step 4 - load lion data and wild dog data with coordinates and time
setwd(choose.dir())
lions <- read.csv("jess.csv",header=T)

# Step 5 - Convert predator data time to the correct format
time.lion <- as.POSIXct(strptime(
  paste(lions$Date, lions$Time, sep=" "), 
  format="%Y/%m/%d %H:%M:%S"),  tz="Africa/Johannesburg")
lions2 <- data.frame(lions,time.lion)

# Step 6 - Convert predator data GPS coordinates to correct format
proj.lion <- project(as.matrix(lions[,c("Longitude","Latitude")]),"+proj=utm +south +zone=36 +ellps=WGS84")
lions3 <- data.frame(lions2,proj.lion)

# Step 7 - Work out the distance to all predator points
distance <- NULL
for(i in 1:length(sight3[,1])){
  first <- sight3[i,"proj.sight"]
  temp1 <- as.numeric(round(distances(centre.xy = first, destmat = lions$proj.lion, verbose = T),1))
  distance <- c(distance,temp1)
}

# Step 8 - Calculate the time difference between sighting and predator locations
elapsed <- NULL
for(i in 1:length(sight3[,1])){
  temp1 <- sight3[i,"time.sight"] - lions3[,"time.lion"]
  temp2 <- abs(temp1)
  elapsed <- c(elapsed,min(temp2))
}

# Step 9 - Output all the sightings data with these 4 new columns
sight4 <- data.frame(sight3,distance,elapsed)