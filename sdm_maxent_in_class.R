# Requirements for posit.cloud
#   RAM to at least 5GB
#   In project file, select NO for all fields in General

# dependencies
install.packages("dismo")
install.packages("maptools")
install.packages("tidyverse")
install.packages("rJava")
install.packages("maps")

library(dismo)
library(maptools)
library(tidyverse)
library(rJava)
library(maps)

### Section 1: Obtaining and Formatting Occurence / Climate Data ### 

# read occurrence data (stop to talk about camelCase)

# convert to spatial points, necessary for modelling and mapping

# obtain climate data: use get data only once
getData("worldclim", var="bio", res=2.5) # current data
# see what each variable is here: https://www.worldclim.org/data/bioclim.html#:~:text=The%20bioclimatic%20variables%20represent%20annual,the%20wet%20and%20dry%20quarters).
#?raster::getData

# create a list of the files in wc2-5 filder so we can make a raster stack
# define raster: https://desktop.arcgis.com/en/arcmap/latest/manage-data/raster-and-images/what-is-raster-data.htm 
#          raster consists of a matrix of cells (or pixels) organized into rows and columns
#         (or a grid) where each cell contains a value representing information, such as 
#         temperature. Rasters are digital aerial photographs, imagery from satellites, 
#         digital pictures, or even scanned maps.
climList <- list.files(path = "wc2-5/", pattern = ".bil$", 
                       full.names = T)  # '..' leads to the path above the folder where the .rmd file is located

# stacking the bioclim variables to process them at one go





### Section 2: Adding Pseudo-Absence Points ### 
# Create pseudo-absence points (making them up, using 'background' approach)
# first we need a raster layer to make the points up on, just picking 1

mask <- raster(clim[[1]]) # mask is the raster object that determines the area where we are generating pts

# determine geographic extent of our data (so we generate random points reasonably nearby)
geographicExtent <- extent(x = ranaDataSpatialPts)

# Random points for background (same number as our observed points we will use )
set.seed(7536) # seed set so we get the same background points each time we run this code 
backgroundPoints <- randomPoints(mask = mask, 
                                 n = nrow(ranaDataNotCoords), # n should be same n as in the pts to be used to test
                                 ext = geographicExtent, 
                                 extf = 1.25, # draw a slightly larger area than where our sp was found (ask katy what is appropriate here)
                                 warn = 0) # don't complain about not having a coordinate reference system

# add col names (can click and see right now they are x and y)
colnames(backgroundPoints) <- c("longitude", "latitude")

### Section 3: Collate Env Data and Point Data into Proper Model Formats ### 
# Data for observation sites (presence and background), with climate data
occEnv <- na.omit(raster::extract(x = clim, y = ranaDataNotCoords)) 
absenceEnv<- na.omit(raster::extract(x = clim, y = backgroundPoints)) # again, many NA values

# Create data frame with presence training data and backround points (0 = abs, 1 = pres)
presenceAbsenceV <- c(rep(1, nrow(occEnv)), rep(0, nrow(absenceEnv)))
presenceAbsenceEnvDf <- as.data.frame(rbind(occEnv, absenceEnv)) 

### Section 4: Create SDM with Maxent ### 
# create a new folder called maxent_outputs


# view the maxent model by navigating in maxent_outputs folder for the html

### Section 5: Plot the Model ###
# clim is huge and it isn't reasonable to predict over whole world
# first we will make it smaller



# for ggplot, we need the prediction to be a data frame 
raster.spdf <- as(ranaPredictPlot, "SpatialPixelsDataFrame")
ranaPredictDf <- as.data.frame(raster.spdf)

# plot in ggplot
wrld <- ggplot2::map_data("world")

xmax <- max(ranaPredictDf$x)
xmin <- min(ranaPredictDf$x)
ymax <- max(ranaPredictDf$y)
ymin <- min(ranaPredictDf$y)









