# dependencies

install.packages("dismo")
install.packages("maptools")
install.packages("tidyverse")
install.packages("maxnet")
install.packages("rJava")
install.packages("terra")

library(dismo)
library(maptools)
library(tidyverse)
library(maxnet)
library(rJava)
library(terra)

# world map data
data(wrld_simpl)

# read occurence data
ranaData <- read_csv("ranaData.csv")
ranaData <- ranaData %>% dplyr::select(longitude, latitude)
ranaDataNotCoords <- read_csv("ranaData.csv") %>% dplyr::select(longitude, latitude)

# convert one data set (ranaData) to spatial
ranaDataSpatialPts <- SpatialPoints(ranaData, proj4string = CRS("+proj=longlat"))

# climate data: use get data only once
getData("worldclim", var="bio", res=2.5) #not the correct name I think for the variable

clim_list <- list.files(path = "wc2-5/", pattern = ".bil$", 
                        full.names = T)  # '..' leads to the path above the folder where the .rmd file is located

# stacking the bioclim variables to process them at one go
clim <- raster::stack(clim_list)

plot(clim[[1]]) # show that it is the first env layer ( = ?)
plot(ranaDataSpatialPts, add = TRUE) 


# determine geographic extent of our data
geographic.extent <- extent(x = ranaDataSpatialPts)


# Create pseudo-absence points (making them up, using 'background' approach)
# first we need a raster layer to make the points up on, just picking 1
bil.files <- list.files(path = "wc2-5", 
                        pattern = "*.bil$", 
                        full.names = TRUE)
mask <- raster(bil.files[1])

# Random points for background (same number as our observed points)
set.seed(19470909) # seed set so we get the same background points each time we run this code 

# raster package will complain about not having coordinate reference system,
# so we suppress that warning
# extf makes the etent of the random points slightly larger than the given ext
background.points <- randomPoints(mask = mask, 
                                  n = nrow(ranaDataNotCoords),
                                  ext = geographic.extent, 
                                  extf = 1.25,
                                  warn = 0)
# add col names (can click and see right now they are x and y)
colnames(background.points) <- c("longitude", "latitude")

# don't eparte in training and testing
# separate presence data into training model and testing model
# randomly select 50% of data for training
set.seed(98)
selected <- sample(1:nrow(ranaDataNotCoords), (0.5 * nrow(ranaDataNotCoords)))
occ_train <- ranaDataNotCoords[selected, ]  # this is the selection to be used for model training
occ_test <- ranaDataNotCoords[-selected, ]  # this is the opposite of the selection which will be used for model testing

# Data for observation sites (presence and background), with climate data
# run each separately, wait until finished at 2.5 gb RAM
occ.train.values <- raster::extract(x = clim, y = ranaDataNotCoords) # why are there NA values ? all of the ranaD. has values
occ.test.values <- raster::extract(x = clim, y = occ_test) # why are there NA values ? all of the ranaD. has values
absence.values <- raster::extract(x = clim, y = background.points)

occ.train.values2 <- na.omit(occ.train.values)
occ.test.values2 <- na.omit(occ.test.values)
absence.values2 <- na.omit(absence.values)

testing.Nas <- cbind(occ_train, occ.train.values)
testing.Nas2 <- cbind(occ_test, occ.test.values)



# Create data frame with presence training data and backround points (0 = abs, 1 = pres)
presence.absence.vector <- c(rep(1, nrow(occ.train.values2)), rep(0, nrow(absence.values)))
presence.absence.train.env.data <- as.data.frame(rbind(occ.train.values2, absence.values)) 
# Mila: dimensions of these objects good bc = 2n(.5 * ranaData?)

## with dismo (java)
ranaModelDismo <- dismo::maxent(x = presence.absence.train.env.data, ## env conditions
                                p = presence.absence.vector,   ## 1:presence or 0:absence
                                path=paste0("maxent_outputs"), ## folder for maxent output; 
                                # if we do not specify a folder R will put the results in a temp file, 
                                # and it gets messy to read those. . .
                                args=c("responsecurves") ## parameter specification
)

# view the maxent model 
ranaModelDismo
plot(ranaModelDismo)
response(ranaModelDismo)

predictExtent <- 1.25 * geographic.extent # choose here what is reasonable for your pts


# graph it
geographicArea <- crop(clim, predictExtent)

ranaPredictPlot <- raster::predict(ranaModelDismo, geographicArea) 

plot(ranaPredictPlot)



## with maxnet
# train Maxent with tabular data
ranaModel <- maxnet::maxnet(data = presence.absence.train.env.data, ## env conditions
                            p = presence.absence.vector,
                            f = maxnet.formula(p = presence.absence.vector,
                                               data = presence.absence.train.env.data,
                                               classes = "lqht"))
## parameter specification
#)
f=maxnet.formula(p=p, data=data, classes='lq')
ranaModel
plot(ranaModel)
#response(ranaModel)
#maxnet::response.plot(ranaModel)

# the maxent functions runs a model in the default settings. To change these parameters,
# you have to tell it what you want...i.e. response curves or the type of features


# predict from terra
terraPredictPlot <- terra::predict(geographicArea, ranaModelDismo)

plot(terraPredictPlot)


terraPredictPlotMaxnet <- terra::predict(geographicArea, ranaModel)

plot(terraPredictPlotMaxnet, main='Maxent, raw values')
plot(wrld_simpl, add=TRUE, border='black')
points(occ_train, pch='+')

str(terraPredictPlotMaxnet)

raster = as.data.frame(terraPredictPlotMaxnet)
ggplot() +
  geom_raster(data = r.df, aes(x = x, y = y, fill = layer)) + 
  coord_quickmap()

r.spdf <- as(terraPredictPlotMaxnet, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
head(r.df)



