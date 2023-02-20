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
ranaDataNotCoords <- read_csv("ranaData.csv") %>% dplyr::select(longitude,latitude)

# convert to spatial points, necessary for modelling and mapping
ranaDataSpatialPts <- SpatialPoints(ranaDataNotCoords, proj4string = CRS("+proj=longlat"))

# obtain climate data: use get data only once
#getData("worldclim", var="bio", res=2.5) # current data
# see what each variable is here: https://www.worldclim.org/data/bioclim.html#:~:text=The%20bioclimatic%20variables%20represent%20annual,the%20wet%20and%20dry%20quarters).
#getData(name = "CMIP5", var = "bio", res = 2.5, year = 70, model = "IP", rcp = 85) # future data
# ASK KATY: WHAT FUTURE PARAMETERS ARE GOOD
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
clim <- raster::stack(climList)

plot(clim[[12]]) # show one env layer ( = annual percepitation) (sorry not using ggplot here just for speed)
plot(ranaDataSpatialPts, add = TRUE) # looks good, we can see where our data is


### Section 2: Adding Pseudo-Absence Points ### 
# Create pseudo-absence points (making them up, using 'background' approach)
# first we need a raster layer to make the points up on, just picking 1

mask <- raster(clim[[1]]) # mask is the raster object that determines the area where we are generating pts

# determine geographic extent of our data (so we generate random points reasonably nearby)
geographicExtent <- extent(x = ranaDataSpatialPts)

# Random points for background (same number as our observed points we will use )
set.seed(3489) # seed set so we get the same background points each time we run this code 
backgroundPoints <- randomPoints(mask = mask, 
                                  n = 0.5 * nrow(ranaDataNotCoords), # n should be same n as in the pts to be used to test
                                  ext = geographicExtent, 
                                  extf = 1.25, # draw a slightly larger area than where our sp was found
                                  warn = 0) # don't complain about not having a coordinate reference system

# add col names (can click and see right now they are x and y)
colnames(backgroundPoints) <- c("longitude", "latitude")

### Section 3: Creating Training and Testing Data Sets ###
# separate presence data into training model and testing model
# randomly select 50% of data for training
set.seed(78)
selected <- sample(1:nrow(ranaDataNotCoords), (0.5 * nrow(ranaDataNotCoords)))
occTrain <- ranaDataNotCoords[selected, ]  # this is the selection to be used for model training
occTest <- ranaDataNotCoords[-selected, ]  # this is the opposite of the selection which will be used for model testing

### Section 4: Collate Env Data and Point Data into Proper Model Formats ### 
# Data for observation sites (presence and background), with climate data
occTrainEnv <- na.omit(raster::extract(x = clim, y = occTrain)) # why are there NA values ? all of the ranaD. has values ASK KATY
occTestEnv <- na.omit(raster::extract(x = clim, y = occTest)) # why are there NA values ? all of the ranaD. has values
absenceEnv<- na.omit(raster::extract(x = clim, y = backgroundPoints)) # again, many NA values

# Create data frame with presence training data and backround points (0 = abs, 1 = pres)
presenceAbsenceV <- c(rep(1, nrow(occTrainEnv)), rep(0, nrow(absenceEnv)))
presenceAbsenceEnvDf <- as.data.frame(rbind(occTrainEnv, absenceEnv)) 


### Section 5: Create SDM with Maxent ### 
# create a new folder called maxent_outputs
ranaSDM <- dismo::maxent(x = presenceAbsenceEnvDf, ## env conditions
                                p = presenceAbsenceV,   ## 1:presence or 0:absence
                                path=paste0("maxent_outputs"), ## folder for maxent output; 
                                # if we do not specify a folder R will put the results in a temp file, 
                                # and it gets messy to read those. . .
                                args=c("responsecurves") ## parameter specification: ASK KATY
)

# view the maxent model by navigating in maxent_outputs folder for the html
plot(ranaSDM) # shows the variable contribution of each one. which one contributes the most to this sp? what does that correp to? 
response(ranaSDM) # . The curves show how the predicted probability of presence changes 
#           as each environmental variable is varied, keeping all other environmental variables at their average sample value.

### Section 6: Plot the Model ###
# clim is huge and it isn't reasonable to predict over whole world
# first we will make it smaller

predictExtent <- 1.25 * geographicExtent # choose here what is reasonable for your pts
geographicArea <- crop(clim, predictExtent) # crop clim to the extent of the map you want
ranaPredictPlot <- raster::predict(ranaSDM, geographicArea) # predict the model to 

# for ggplot, we need the prediction to be a data frame 
raster.spdf <- as(ranaPredictPlot, "SpatialPixelsDataFrame")
ranaPredictDf <- as.data.frame(raster.spdf)

# plot in ggplot
wrld <- ggplot2::map_data("world")
xmax <- max(ranaPredictDf$x)
xmin <- min(ranaPredictDf$x)
ymax <- max(ranaPredictDf$y)
ymin <- min(ranaPredictDf$y)


ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDf, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  scale_size_area() +
  borders("state") +
  labs(title = "SDM of R. boylii Under Current Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Probability of Presence") +
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) +
  geom_point(data = ranaDataNotCoords, mapping = aes(x = longitude, y = latitude))
  
  # not sure why there is a margin ... working on it




### Section 7: Evaluate Model ### 





